module Tracker.Services.ConfigReader 
  ( ConfigReader(..)
  , mkConfigReader
  ) where

import Tracker.Models.AppConfig

import Prelude
import Dhall
import Control.Monad.IO.Class
import qualified Data.Text as T
import RIO

data ConfigReader f = ConfigReader
  { read :: f AppConfig 
  }

mkConfigReader :: (MonadIO f) => Maybe String -> ConfigReader f
mkConfigReader maybePath = ConfigReader $ liftIO $ input auto path
  where path = T.pack $ fromMaybe "./tracker/resources/config.dhall" maybePath