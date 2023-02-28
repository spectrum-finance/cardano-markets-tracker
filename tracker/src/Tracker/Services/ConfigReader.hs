module Tracker.Services.ConfigReader 
  ( ConfigReader(..)
  , mkConfigReader
  ) where

import Prelude
import Dhall 
  ( auto, input )
import RIO 
  ( MonadIO(..), fromMaybe )

import qualified Data.Text as T

import Tracker.Models.AppConfig 
  ( AppConfig )

data ConfigReader f = ConfigReader
  { read :: f AppConfig 
  }

mkConfigReader :: (MonadIO f) => Maybe String -> ConfigReader f
mkConfigReader maybePath = ConfigReader $ liftIO $ input auto path
  where path = T.pack $ fromMaybe "./tracker/resources/config.dhall" maybePath