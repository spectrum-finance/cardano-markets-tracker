module Tracker.App
  ( App(..)
  , mkApp
  ) where

import Streaming.Producer

import Explorer.Service

import Tracker.Programs.TrackerProgram
import Tracker.Models.AppConfig
import Tracker.Services.ConfigReader
import Tracker.Services.Tracker
import Tracker.Caches.Cache

import           System.Logging.Hlog
import           RIO
import qualified Streamly.Prelude as S
import           Control.Monad.Trans.Resource
import           Control.Monad.Catch
import           Kafka.Producer
import           RIO.List

data App = App
  { runApp :: IO ()
  }

mkApp :: [String] -> IO App
mkApp args = return $ App $ wire args

wire :: (MonadUnliftIO f, MonadCatch f, S.MonadAsync f, MonadMask f) => [String] -> f ()
wire args = runResourceT $ do
  AppConfig{..}    <- lift $ read $ mkConfigReader (headMaybe args)
  loggingMaker     <- makeLogging loggingConfig
  ordersProducer   <- mkKafkaProducer ordersProducerConfig (TopicName ordersTopicName)
  poolsProducer    <- mkKafkaProducer poolsProducerConfig (TopicName poolsTopicName)
  trackerCache     <- mkCache redisSettings loggingMaker
  explorer         <- mkExplorer loggingMaker explorerConfig
  trackerService   <- mkTrackerService trackerSettings retry loggingMaker trackerCache explorer
  trackerProgramm  <- mkTrackerProgram trackerProgrammConfig loggingMaker trackerCache trackerService ordersProducer poolsProducer
  lift $ run trackerProgramm