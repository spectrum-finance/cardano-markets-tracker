{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Tracker.App
  ( App(..)
  , runApp
  ) where

import RIO
  ( ReaderT (..), MonadReader (ask), MonadIO (liftIO), Alternative (..), MonadPlus (..), (<&>), void )

import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Control.Concurrent.STM.TMVar   as STM
import qualified Control.Concurrent.STM.TQueue  as STM
import qualified Control.Concurrent.STM.TVar    as STM
import qualified Control.Monad.STM              as STM
import qualified Control.Concurrent.Async       as Async

import Prelude hiding (read)

import Data.Aeson
  ( encode )
import Data.ByteString.Lazy.UTF8
  ( toString )
import RIO.List
  ( headMaybe )
import Dhall
  ( Generic, Text )
import Streamly.Prelude as S
  ( drain, IsStream, fromList, mapM, SerialT, parallel )
import Crypto.Random.Types
  ( MonadRandom(..) )
import Control.Tracer
  ( stdoutTracer, Contravariant (contramap) )
import Control.Monad.Class.MonadSTM
  ( MonadSTM (..) )
import Control.Monad.Class.MonadST
  ( MonadST )
import Control.Monad.Class.MonadAsync
  ( MonadAsync (..) )
import Control.Monad.Class.MonadFork
  ( MonadThread, MonadFork )
import Control.Monad.Trans.Control
  ( MonadBaseControl )
import Control.Monad.Base
  ( MonadBase )
import Control.Monad.Trans.Class
  ( MonadTrans(lift) )
import Crypto.Random.Entropy
  ( getEntropy )
import qualified Control.Monad.Catch as MC

import Spectrum.EventSource.Data.TxContext
    ( TxCtx(LedgerCtx, MempoolCtx) )

import System.Posix.Signals
  ( Handler (..)
  , installHandler
  , keyboardSignal
  , raiseSignal
  , softwareTermination
  )

import Spectrum.LedgerSync.Config
  ( NetworkParameters, parseNetworkParameters, NodeSocketConfig )
import Cardano.Network.Protocol.NodeToClient.Trace
  ( encodeTraceClient )

import Streaming.Producer
  ( mkKafkaProducer, Producer(produce) )
import Control.Monad.Class.MonadThrow
  ( MonadThrow, MonadMask, MonadCatch )

import System.Logging.Hlog
    ( translateMakeLogging, makeLogging, Logging(..), MakeLogging(forComponent) )
import Control.Monad.Trans.Resource
    ( MonadUnliftIO, runResourceT, ResourceT )
import Kafka.Producer
  ( TopicName(TopicName) )

import Spectrum.LedgerSync
  ( mkLedgerSync, LedgerSync )
import Spectrum.EventSource.Stream
  ( mkLedgerEventSource, EventSource(..), mkMempoolTxEventSource )
import Spectrum.Config
  ( EventSourceConfig )
import Spectrum.EventSource.Data.TxEvent
  (TxEvent (..) )
import Streaming.Config
  ( KafkaProducerConfig(..) )
import Spectrum.EventSource.Data.Tx
  ( MinimalTx(..), MinimalUnconfirmedTx (..), MinimalConfirmedTx (..) )
import CardanoTx.Models
  (FullTxOut (..) )
import ErgoDex.Amm.Orders
  (Swap(..), Deposit(..), Redeem(..), AnyOrder (AnyOrder), OrderAction (..) )
import ErgoDex.State
  ( OnChain(..), Confirmed(..) )
import ErgoDex.Class
  ( FromLedger(..) )
import Tracker.Syntax.Option
  ( unNone )
import ErgoDex.Amm.Pool
  ( Pool )
import ErgoDex.ScriptsValidators
  ( ScriptsValidators, mkScriptsValidators, ScriptsConfig, parsePool )
import Tracker.Models.AppConfig
import Cardano.Api (SlotNo)
import Tracker.Models.OnChainEvent (OnChainEvent (OnChainEvent))
import Spectrum.EventSource.Persistence.Config (LedgerStoreConfig)

newtype App a = App
  { unApp :: ReaderT (Env Wire App) IO a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (Env Wire App)
    , MonadIO
    , MonadST
    , MonadThread, MonadFork
    , MonadThrow, MC.MonadThrow, MonadCatch, MC.MonadCatch, MonadMask, MC.MonadMask
    , MonadBase IO, MonadBaseControl IO, MonadUnliftIO
    )

type Wire = ResourceT App

data Env f m = Env
  { nodeSocketConfig             :: !NodeSocketConfig
  , ledgerStoreConfig            :: !LedgerStoreConfig
  , eventSourceConfig            :: !EventSourceConfig
  , networkParams                :: !NetworkParameters
  , txEventsProducerConfig       :: !KafkaProducerConfig
  , txEventsTopicName            :: !Text
  , ordersProducerConfig         :: !KafkaProducerConfig
  , ordersTopicName              :: !Text
  , mempoolOrdersProducerConfig  :: !KafkaProducerConfig
  , mempoolOrdersTopicName       :: !Text
  , poolsProducerConfig          :: !KafkaProducerConfig
  , poolsTopicName               :: !Text
  , scriptsConfig                :: !ScriptsConfig
  , mkLogging                    :: !(MakeLogging f m)
  , mkLogging'                   :: !(MakeLogging m m)
  } deriving stock (Generic)

runContext :: Env Wire App -> App a -> IO a
runContext env app = runReaderT (unApp app) env

runApp :: [String] -> IO ()
runApp args = do
  AppConfig{..} <- loadAppConfig (headMaybe args)
  nparams       <- parseNetworkParameters nodeConfigPath
  mkLogging     <- makeLogging loggingConfig :: IO (MakeLogging IO App)
  let
    env =
      Env
        nodeSocketConfig
        lederStoreConfig
        eventSourceConfig
        nparams
        txEventsProducerConfig
        txEventsTopicName
        ordersProducerConfig
        ordersTopicName
        mempoolOrdersProducerConfig
        mempoolOrdersTopicName
        poolsProducerConfig
        poolsTopicName
        scriptsConfig
        (translateMakeLogging (lift . App . lift) mkLogging)
        (translateMakeLogging (App . lift) mkLogging)
  runContext env (runResourceT wireApp)

wireApp :: Wire ()
wireApp = do
  env@Env{..} <- ask
  let tr = contramap (toString . encode . encodeTraceClient) stdoutTracer

  lsync   <- lift $ mkLedgerSync (runContext env) tr mkLogging' nodeSocketConfig networkParams -- :: ResourceT App (LedgerSync App)
  lsource <- mkLedgerEventSource lsync lift :: ResourceT App (EventSource S.SerialT App 'LedgerCtx)
  msource <- mkMempoolTxEventSource lsync   :: ResourceT App (EventSource S.SerialT App 'MempoolCtx)

  scriptsValidators      <- lift $ mkScriptsValidators scriptsConfig
  processTxEventsLogging <- forComponent mkLogging "processTxEvents"

  txEventsProducer      <- mkKafkaProducer txEventsProducerConfig (TopicName txEventsTopicName)
  ordersProducer        <- mkKafkaProducer ordersProducerConfig (TopicName ordersTopicName)
  mempoolOrdersProducer <- mkKafkaProducer mempoolOrdersProducerConfig (TopicName mempoolOrdersTopicName)
  poolsProducer         <- mkKafkaProducer poolsProducerConfig (TopicName poolsTopicName)
  lift . S.drain $ 
    S.parallel (processTxEvents processTxEventsLogging scriptsValidators (upstream lsource) txEventsProducer ordersProducer poolsProducer) $
    processMempoolTxEvents processTxEventsLogging (upstream msource) mempoolOrdersProducer

processTxEvents
  ::
    ( IsStream s
    , MonadIO m
    , MonadBaseControl IO m
    , MC.MonadThrow m
    )
  => Logging m
  -> ScriptsValidators
  -> s m (TxEvent ctx)
  -> Producer m String (TxEvent ctx)
  -> Producer m String (OnChainEvent AnyOrder)
  -> Producer m String (OnChainEvent Pool)
  -> s m ()
processTxEvents logging scriptsValidators txEventsStream txEventsProducer ordersProducer poolProducer =
  S.mapM (\txEvent -> do
      produce txEventsProducer (S.fromList [(mkKafkaKey txEvent, txEvent)])
      parseOrders logging txEvent >>= write2Kafka ordersProducer
      parsePools  logging scriptsValidators txEvent >>= write2Kafka poolProducer
    ) txEventsStream

processMempoolTxEvents
  ::
    ( IsStream s
    , MonadIO m
    , MonadBaseControl IO m
    , MC.MonadThrow m
    )
  => Logging m
  -> s m (TxEvent ctx)
  -> Producer m String (OnChainEvent AnyOrder)
  -> s m ()
processMempoolTxEvents logging txEventsStream mempoolOrdersProducer =
  S.mapM (\txEvent -> do
      parseOrders logging txEvent >>= write2Kafka mempoolOrdersProducer
    ) txEventsStream

write2Kafka :: (Monad m) => Producer m String (OnChainEvent a) -> [OnChainEvent a] -> m ()
write2Kafka producer = produce producer . S.fromList . mkKafkaTuple

parsePools :: forall m ctx. (MonadIO m) => Logging m -> ScriptsValidators -> TxEvent ctx -> m [OnChainEvent Pool]
parsePools logging scriptsValidators (AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..})) =
 (parsePool logging scriptsValidators `traverse` txOutputs) <&> unNone <&> (\confirmedList -> (\(Confirmed _ a) -> OnChainEvent a slotNo) <$> confirmedList)
parsePools _  _  _= pure []

mkKafkaTuple :: [OnChainEvent a] -> [(String, OnChainEvent a)]
mkKafkaTuple ordersList = (\event@(OnChainEvent (OnChain FullTxOut{..} _) _) -> (show fullTxOutRef, event)) <$> ordersList

parseOrders :: forall m ctx. (MonadIO m) => Logging m -> TxEvent ctx -> m [OnChainEvent AnyOrder]
parseOrders logging (AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..})) =
 (parseOrder logging slotNo `traverse` txOutputs) <&> unNone
parseOrders logging (PendingTx (MinimalMempoolTx MinimalUnconfirmedTx{..})) =
  (parseOrder logging slotNo `traverse` txOutputs) <&> unNone
parseOrders _  _ = pure []

parseOrder :: (MonadIO m) => Logging m -> SlotNo -> FullTxOut -> m (Maybe (OnChainEvent AnyOrder))
parseOrder Logging{..} slot out =
  let
    swap    = parseFromLedger @Swap out
    deposit = parseFromLedger @Deposit out
    redeem  = parseFromLedger @Redeem out
  in case (swap, deposit, redeem) of
    (Just (OnChain _ swap'), _, _)    -> do
      infoM ("Swap order: " ++ show swap)
      pure . Just $ OnChainEvent (OnChain out $ AnyOrder (swapPoolId swap') (SwapAction swap')) slot
    (_, Just (OnChain _ deposit'), _) -> do
      infoM ("Deposit order: " ++ show deposit)
      pure .  Just $ OnChainEvent (OnChain out $ AnyOrder (depositPoolId deposit') (DepositAction deposit')) slot
    (_, _, Just (OnChain _ redeem'))  -> do
      infoM ("Redeem order: " ++ show redeem)
      pure .  Just $ OnChainEvent (OnChain out $ AnyOrder (redeemPoolId redeem') (RedeemAction redeem')) slot
    _                                 -> do
      infoM ("Order not found in: " ++ show (fullTxOutRef out))
      pure Nothing

mkKafkaKey :: TxEvent ctx -> String
mkKafkaKey (PendingTx (MinimalMempoolTx MinimalUnconfirmedTx{..})) = show txId
mkKafkaKey (AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..})) = show txId
mkKafkaKey (UnappliedTx txId) = show txId

instance MonadRandom App where
    getRandomBytes = liftIO . getEntropy

newtype WrappedSTM a = WrappedSTM { unwrapSTM :: STM.STM a }
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadThrow)

instance MonadSTM App where
  type STM     App = WrappedSTM
  type TVar    App = STM.TVar
  type TMVar   App = STM.TMVar
  type TQueue  App = STM.TQueue
  type TBQueue App = STM.TBQueue

  atomically      = App . lift . STM.atomically . unwrapSTM
  retry           = WrappedSTM STM.retry
  orElse          = \a0 a1 -> WrappedSTM (STM.orElse (unwrapSTM a0) (unwrapSTM a1))
  check           = WrappedSTM . STM.check

  newTVar         = WrappedSTM . STM.newTVar
  newTVarIO       = App . lift . STM.newTVarIO
  readTVar        = WrappedSTM . STM.readTVar
  readTVarIO      = App . lift . STM.readTVarIO
  writeTVar       = \a0 -> WrappedSTM . STM.writeTVar a0
  modifyTVar      = \a0 -> WrappedSTM . STM.modifyTVar a0
  modifyTVar'     = \a0 -> WrappedSTM . STM.modifyTVar' a0
  stateTVar       = \a0 -> WrappedSTM . STM.stateTVar a0
  swapTVar        = \a0 -> WrappedSTM . STM.swapTVar a0

  newTMVar        = WrappedSTM . STM.newTMVar
  newTMVarIO      = App . lift . STM.newTMVarIO
  newEmptyTMVar   = WrappedSTM STM.newEmptyTMVar
  newEmptyTMVarIO = App (lift STM.newEmptyTMVarIO)
  takeTMVar       = WrappedSTM . STM.takeTMVar
  tryTakeTMVar    = WrappedSTM . STM.tryTakeTMVar
  putTMVar        = \a0 -> WrappedSTM . STM.putTMVar a0
  tryPutTMVar     = \a0 -> WrappedSTM . STM.tryPutTMVar a0
  readTMVar       = WrappedSTM . STM.readTMVar
  tryReadTMVar    = WrappedSTM . STM.tryReadTMVar
  swapTMVar       = \a0 -> WrappedSTM . STM.swapTMVar a0
  isEmptyTMVar    = WrappedSTM . STM.isEmptyTMVar

  newTQueue       = WrappedSTM STM.newTQueue
  newTQueueIO     = App (lift STM.newTQueueIO)
  readTQueue      = WrappedSTM . STM.readTQueue
  tryReadTQueue   = WrappedSTM . STM.tryReadTQueue
  peekTQueue      = WrappedSTM . STM.peekTQueue
  tryPeekTQueue   = WrappedSTM . STM.tryPeekTQueue
  flushTBQueue    = WrappedSTM . STM.flushTBQueue
  writeTQueue     = \a0 -> WrappedSTM . STM.writeTQueue a0
  isEmptyTQueue   = WrappedSTM . STM.isEmptyTQueue

  newTBQueue      = WrappedSTM . STM.newTBQueue
  newTBQueueIO    = App . lift . STM.newTBQueueIO
  readTBQueue     = WrappedSTM . STM.readTBQueue
  tryReadTBQueue  = WrappedSTM . STM.tryReadTBQueue
  peekTBQueue     = WrappedSTM . STM.peekTBQueue
  tryPeekTBQueue  = WrappedSTM . STM.tryPeekTBQueue
  writeTBQueue    = \a0 -> WrappedSTM . STM.writeTBQueue a0
  lengthTBQueue   = WrappedSTM . STM.lengthTBQueue
  isEmptyTBQueue  = WrappedSTM . STM.isEmptyTBQueue
  isFullTBQueue   = WrappedSTM . STM.isFullTBQueue

newtype WrappedAsync a = WrappedAsync { unwrapAsync :: Async.Async a }
    deriving newtype (Functor)

instance MonadAsync App where
  type Async App  = WrappedAsync
  async           = \(App (ReaderT m)) -> App (ReaderT $ \r -> WrappedAsync <$> async (m r))
  asyncThreadId   = Async.asyncThreadId . unwrapAsync
  pollSTM         = WrappedSTM . Async.pollSTM . unwrapAsync
  waitCatchSTM    = WrappedSTM . Async.waitCatchSTM . unwrapAsync
  cancel          = App . lift . Async.cancel . unwrapAsync
  cancelWith      = \a0 -> App . lift . Async.cancelWith (unwrapAsync a0)
  asyncWithUnmask = \restore -> App $ ReaderT $ \r ->
      fmap WrappedAsync $ Async.asyncWithUnmask $ \unmask ->
        runReaderT (unApp (restore (liftF unmask))) r
    where
      liftF :: (IO a -> IO a) -> App a -> App a
      liftF g (App (ReaderT f)) = App (ReaderT (g . f))