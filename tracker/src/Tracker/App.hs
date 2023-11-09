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
import Tracker.Models.LBSPDatum (LBSPDatum)

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
  , txEventsLedgerProducerConfig       :: !KafkaProducerConfig
  , txEventsLedgerTopicName            :: !Text
  , txEventsMempoolProducerConfig       :: !KafkaProducerConfig
  , txEventsMempoolTopicName         :: !Text
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
        txEventsLedgerProducerConfig
        txEventsLedgerTopicName
        txEventsMempoolProducerConfig
        txEventsMempoolTopicName
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

  processTxEventsLogging <- forComponent mkLogging "processTxEvents"

  txEventsProducerLedger  <- mkKafkaProducer txEventsLedgerProducerConfig (TopicName txEventsLedgerTopicName)
  txEventsProducerMempool <- mkKafkaProducer txEventsMempoolProducerConfig (TopicName txEventsMempoolTopicName)
  lift . S.drain $ 
    S.parallel (processTxEvents processTxEventsLogging (upstream lsource) txEventsProducerLedger) $
    processMempoolTxEvents processTxEventsLogging (upstream msource) txEventsProducerMempool

processTxEvents
  ::
    ( IsStream s
    , MonadIO m
    , MonadBaseControl IO m
    , MC.MonadThrow m
    )
  => Logging m
  -> s m (TxEvent ctx)
  -> Producer m String (TxEvent ctx)
  -> s m ()
processTxEvents logging txEventsStream txEventsProducer =
  S.mapM (\txEvent -> do
      produce txEventsProducer (S.fromList [(mkKafkaKey txEvent, txEvent)])
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
  -> Producer m String (TxEvent ctx)
  -> s m ()
processMempoolTxEvents logging txEventsStream txEventsProducer =
  S.mapM (\txEvent -> do
      produce txEventsProducer (S.fromList [(mkKafkaKey txEvent, txEvent)])
    ) txEventsStream

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