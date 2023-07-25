module Tracker.Models.AppConfig where

import Dhall 
  ( Generic, Text, FromDhall, auto, input )
import RIO 
  ( fromMaybe, MonadIO(..) )

import qualified Data.Text as T

import System.Logging.Hlog 
  ( LoggingConfig )
import Streaming.Config 
  ( KafkaProducerConfig )

import Spectrum.Config 
  ( EventSourceConfig )
import Spectrum.EventSource.Persistence.Config
  ( LedgerStoreConfig )
import ErgoDex.ScriptsValidators 
  ( ScriptsConfig )
import Spectrum.LedgerSync.Config

data AppConfig = AppConfig
  { nodeSocketConfig             :: !NodeSocketConfig
  , ordersProducerConfig         :: !KafkaProducerConfig
  , poolsProducerConfig          :: !KafkaProducerConfig
  , txEventsProducerConfig       :: !KafkaProducerConfig
  , mempoolOrdersProducerConfig  :: !KafkaProducerConfig
  , nodeConfigPath               :: !FilePath
  , mempoolOrdersTopicName       :: !Text 
  , ordersTopicName              :: !Text
  , poolsTopicName               :: !Text
  , txEventsTopicName            :: !Text
  , loggingConfig                :: !LoggingConfig
  , eventSourceConfig            :: !EventSourceConfig
  , lederStoreConfig             :: !LedgerStoreConfig
  , scriptsConfig                :: !ScriptsConfig
  } deriving (Generic)

instance FromDhall AppConfig

loadAppConfig :: MonadIO f => Maybe String -> f AppConfig
loadAppConfig maybePath = liftIO $ input auto path
  where path = T.pack $ fromMaybe "./tracker/resources/config.dhall" maybePath