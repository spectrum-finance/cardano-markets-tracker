{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Tracker.App
  ( App(..)
  , runApp1
  ) where

import RIO
  ( ReaderT (..), MonadReader (ask), MonadIO (liftIO), Alternative (..), MonadPlus (..), (<&>) )

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
  ( drain, IsStream, fromList, mapM, SerialT )
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
    ( TxCtx(LedgerCtx) )

import Spectrum.LedgerSync.Config
  ( NetworkParameters, LedgerSyncConfig, parseNetworkParameters )
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
  ( mkEventSource, EventSource(..) )
import Spectrum.Config 
  ( EventSourceConfig )
import Spectrum.EventSource.Persistence.Config 
  ( LedgerStoreConfig )
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

data App = App
  { runApp :: IO ()
  }

newtype AppNew a = AppNew
  { unApp :: ReaderT (Env Wire AppNew) IO a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (Env Wire AppNew)
    , MonadIO
    , MonadST
    , MonadThread, MonadFork
    , MonadThrow, MC.MonadThrow, MonadCatch, MC.MonadCatch, MonadMask, MC.MonadMask
    , MonadBase IO, MonadBaseControl IO, MonadUnliftIO
    )

type Wire = ResourceT AppNew

data Env f m = Env
  { ledgerSyncConfig       :: !LedgerSyncConfig
  , eventSourceConfig      :: !EventSourceConfig
  , networkParams          :: !NetworkParameters
  , lederHistoryConfig     :: !LedgerStoreConfig
  , txEventsProducerConfig :: !KafkaProducerConfig
  , txEventsTopicName      :: !Text
  , ordersProducerConfig   :: !KafkaProducerConfig
  , ordersTopicName        :: !Text
  , poolsProducerConfig    :: !KafkaProducerConfig
  , poolsTopicName         :: !Text
  , scriptsConfig          :: !ScriptsConfig
  , mkLogging              :: !(MakeLogging f m)
  , mkLogging'             :: !(MakeLogging m m)
  } deriving stock (Generic)

runContext :: Env Wire AppNew -> AppNew a -> IO a
runContext env app = runReaderT (unApp app) env

runApp1 :: [String] -> IO ()
runApp1 args = do
  AppConfig{..} <- loadAppConfig (headMaybe args)
  nparams       <- parseNetworkParameters nodeConfigPath
  mkLogging     <- makeLogging loggingConfig :: IO (MakeLogging IO AppNew)
  let
    env =
      Env
        ledgerSyncConfig
        eventSourceConfig
        nparams
        lederHistoryConfig
        txEventsProducerConfig
        txEventsTopicName
        ordersProducerConfig
        ordersTopicName
        poolsProducerConfig
        poolsTopicName
        scriptsConfig
        (translateMakeLogging (lift . AppNew . lift) mkLogging)
        (translateMakeLogging (AppNew . lift) mkLogging)
  runContext env (runResourceT wireApp)

wireApp :: Wire ()
wireApp = do
  env@Env{..} <- ask
  let tr = contramap (toString . encode . encodeTraceClient) stdoutTracer

  lsync   <- lift $ mkLedgerSync (runContext env) tr :: ResourceT AppNew (LedgerSync AppNew)
  lsource <- mkEventSource lsync :: ResourceT AppNew (EventSource S.SerialT AppNew 'LedgerCtx)

  scriptsValidators      <- lift $ mkScriptsValidators scriptsConfig
  processTxEventsLogging <- forComponent mkLogging "processTxEvents"

  txEventsProducer <- mkKafkaProducer txEventsProducerConfig (TopicName txEventsTopicName)
  ordersProducer   <- mkKafkaProducer ordersProducerConfig (TopicName ordersTopicName)
  poolsProducer    <- mkKafkaProducer poolsProducerConfig (TopicName poolsTopicName)
  lift . S.drain $ processTxEvents processTxEventsLogging scriptsValidators (upstream lsource) txEventsProducer ordersProducer poolsProducer

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
  -> Producer m String (OnChain AnyOrder)
  -> Producer m String (OnChain Pool)
  -> s m ()
processTxEvents logging scriptsValidators txEventsStream txEventsProducer ordersProducer poolProducer =
  S.mapM (\txEvent -> do
      produce txEventsProducer (S.fromList [(mkKafkaKey txEvent, txEvent)])
      parseOrders logging txEvent >>= write2Kafka ordersProducer
      parsePools  logging scriptsValidators txEvent >>= write2Kafka poolProducer
    ) txEventsStream

write2Kafka :: (Monad m) => Producer m String (OnChain a) -> [OnChain a] -> m ()
write2Kafka producer = produce producer . S.fromList . mkKafkaTuple 

parsePools :: forall m ctx. (MonadIO m) => Logging m -> ScriptsValidators -> TxEvent ctx -> m [OnChain Pool]
parsePools logging scriptsValidators (AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..})) =
 (parsePool logging scriptsValidators `traverse` txOutputs) <&> unNone <&> (\confirmedList -> (\(Confirmed _ a) -> a) <$> confirmedList)
parsePools _  _  _= pure []

mkKafkaTuple :: [OnChain a] -> [(String, OnChain a)]
mkKafkaTuple ordersList = (\order@(OnChain FullTxOut{..} _) -> (show fullTxOutRef, order)) <$> ordersList

parseOrders :: forall m ctx. (MonadIO m) => Logging m -> TxEvent ctx -> m [OnChain AnyOrder]
parseOrders logging (AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..})) =
 (parseOrder logging `traverse` txOutputs) <&> unNone
parseOrders _  _ = pure []

parseOrder :: (MonadIO m) => Logging m -> FullTxOut -> m (Maybe (OnChain AnyOrder))
parseOrder Logging{..} out =
  let
    swap    = parseFromLedger @Swap out
    deposit = parseFromLedger @Deposit out
    redeem  = parseFromLedger @Redeem out
  in case (swap, deposit, redeem) of
    (Just (OnChain _ swap'), _, _)    -> do
      infoM ("Swap order: " ++ show swap)
      pure $ Just . OnChain out $ AnyOrder (swapPoolId swap') (SwapAction swap')
    (_, Just (OnChain _ deposit'), _) -> do
      infoM ("Deposit order: " ++ show deposit)
      pure $  Just . OnChain out $ AnyOrder (depositPoolId deposit') (DepositAction deposit')
    (_, _, Just (OnChain _ redeem'))  -> do
      infoM ("Redeem order: " ++ show redeem)
      pure $  Just . OnChain out $ AnyOrder (redeemPoolId redeem') (RedeemAction redeem')
    _                                 -> do
      infoM ("Order not found in: " ++ show out)
      pure $ Nothing

mkKafkaKey :: TxEvent ctx -> String
mkKafkaKey (PendingTx (MinimalMempoolTx MinimalUnconfirmedTx{..})) = show txId
mkKafkaKey (PendingTx (MinimalLedgerTx MinimalConfirmedTx{..})) = show txId    -- todo: doesn't support in current version
mkKafkaKey (AppliedTx (MinimalMempoolTx MinimalUnconfirmedTx{..})) = show txId -- todo: doesn't support in current version
mkKafkaKey (AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..})) = show txId
mkKafkaKey (UnappliedTx txId) = show txId

instance MonadRandom AppNew where
    getRandomBytes = liftIO . getEntropy

newtype WrappedSTM a = WrappedSTM { unwrapSTM :: STM.STM a }
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadThrow)

instance MonadSTM AppNew where
  type STM     AppNew = WrappedSTM
  type TVar    AppNew = STM.TVar
  type TMVar   AppNew = STM.TMVar
  type TQueue  AppNew = STM.TQueue
  type TBQueue AppNew = STM.TBQueue

  atomically      = AppNew . lift . STM.atomically . unwrapSTM
  retry           = WrappedSTM STM.retry
  orElse          = \a0 a1 -> WrappedSTM (STM.orElse (unwrapSTM a0) (unwrapSTM a1))
  check           = WrappedSTM . STM.check

  newTVar         = WrappedSTM . STM.newTVar
  newTVarIO       = AppNew . lift . STM.newTVarIO
  readTVar        = WrappedSTM . STM.readTVar
  readTVarIO      = AppNew . lift . STM.readTVarIO
  writeTVar       = \a0 -> WrappedSTM . STM.writeTVar a0
  modifyTVar      = \a0 -> WrappedSTM . STM.modifyTVar a0
  modifyTVar'     = \a0 -> WrappedSTM . STM.modifyTVar' a0
  stateTVar       = \a0 -> WrappedSTM . STM.stateTVar a0
  swapTVar        = \a0 -> WrappedSTM . STM.swapTVar a0

  newTMVar        = WrappedSTM . STM.newTMVar
  newTMVarIO      = AppNew . lift . STM.newTMVarIO
  newEmptyTMVar   = WrappedSTM STM.newEmptyTMVar
  newEmptyTMVarIO = AppNew (lift STM.newEmptyTMVarIO)
  takeTMVar       = WrappedSTM . STM.takeTMVar
  tryTakeTMVar    = WrappedSTM . STM.tryTakeTMVar
  putTMVar        = \a0 -> WrappedSTM . STM.putTMVar a0
  tryPutTMVar     = \a0 -> WrappedSTM . STM.tryPutTMVar a0
  readTMVar       = WrappedSTM . STM.readTMVar
  tryReadTMVar    = WrappedSTM . STM.tryReadTMVar
  swapTMVar       = \a0 -> WrappedSTM . STM.swapTMVar a0
  isEmptyTMVar    = WrappedSTM . STM.isEmptyTMVar

  newTQueue       = WrappedSTM STM.newTQueue
  newTQueueIO     = AppNew (lift STM.newTQueueIO)
  readTQueue      = WrappedSTM . STM.readTQueue
  tryReadTQueue   = WrappedSTM . STM.tryReadTQueue
  peekTQueue      = WrappedSTM . STM.peekTQueue
  tryPeekTQueue   = WrappedSTM . STM.tryPeekTQueue
  flushTBQueue    = WrappedSTM . STM.flushTBQueue
  writeTQueue     = \a0 -> WrappedSTM . STM.writeTQueue a0
  isEmptyTQueue   = WrappedSTM . STM.isEmptyTQueue

  newTBQueue      = WrappedSTM . STM.newTBQueue
  newTBQueueIO    = AppNew . lift . STM.newTBQueueIO
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

instance MonadAsync AppNew where
  type Async AppNew  = WrappedAsync
  async           = \(AppNew (ReaderT m)) -> AppNew (ReaderT $ \r -> WrappedAsync <$> async (m r))
  asyncThreadId   = Async.asyncThreadId . unwrapAsync
  pollSTM         = WrappedSTM . Async.pollSTM . unwrapAsync
  waitCatchSTM    = WrappedSTM . Async.waitCatchSTM . unwrapAsync
  cancel          = AppNew . lift . Async.cancel . unwrapAsync
  cancelWith      = \a0 -> AppNew . lift . Async.cancelWith (unwrapAsync a0)
  asyncWithUnmask = \restore -> AppNew $ ReaderT $ \r ->
      fmap WrappedAsync $ Async.asyncWithUnmask $ \unmask ->
        runReaderT (unApp (restore (liftF unmask))) r
    where
      liftF :: (IO a -> IO a) -> AppNew a -> AppNew a
      liftF g (AppNew (ReaderT f)) = AppNew (ReaderT (g . f))