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

import Spectrum.LedgerSync.Config 
  ( LedgerSyncConfig )
import Spectrum.Config 
  ( EventSourceConfig )
import Spectrum.EventSource.Persistence.Config
  ( LedgerStoreConfig )
import ErgoDex.ScriptsValidators 
  ( ScriptsConfig )

data AppConfig = AppConfig
  { ordersProducerConfig   :: !KafkaProducerConfig
  , ledgerSyncConfig       :: !LedgerSyncConfig
  , poolsProducerConfig    :: !KafkaProducerConfig
  , txEventsProducerConfig :: !KafkaProducerConfig
  , nodeConfigPath         :: !FilePath
  , ordersTopicName        :: !Text
  , poolsTopicName         :: !Text
  , txEventsTopicName      :: !Text
  , loggingConfig          :: !LoggingConfig
  , eventSourceConfig      :: !EventSourceConfig
  , lederHistoryConfig     :: !LedgerStoreConfig
  , scriptsConfig          :: !ScriptsConfig
  } deriving (Generic)

instance FromDhall AppConfig

loadAppConfig :: MonadIO f => Maybe String -> f AppConfig
loadAppConfig maybePath = liftIO $ input auto path
  where path = T.pack $ fromMaybe "./tracker/resources/config.dhall" maybePath