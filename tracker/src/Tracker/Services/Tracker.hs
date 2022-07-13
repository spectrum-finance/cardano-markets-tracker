module Tracker.Services.Tracker where

import CardanoTx.Models

import Explorer.Service
import Explorer.Models
import Explorer.Class
import Explorer.Types

import System.Logging.Hlog
import RIO
import RIO.List
import GHC.Natural
import Control.Retry
import Control.Monad.Catch

import Tracker.Syntax.Retry
import Tracker.Caches.Cache
import Tracker.Models.AppConfig
import Tracker.Models.SettledTx

data TrackerService f = TrackerService
  { getAllTransactions :: f ([SettledTx], Int)
  }

mkTrackerService
  :: (Monad i, MonadIO f, MonadMask f)
  => TrackerSettings
  -> RetryConfig
  -> MakeLogging i f
  -> Cache f
  -> Explorer f
  -> i (TrackerService f)
mkTrackerService settings retry MakeLogging{..} cache explorer = do
  logger <- forComponent "trackerService"
  pure $ TrackerService $ getAllTransactions' explorer cache logger settings retry

getAllTransactions' 
  :: (MonadIO f, MonadMask f) 
  => Explorer f
  -> Cache f
  -> Logging f
  -> TrackerSettings
  -> RetryConfig
  -> f ([SettledTx], Int)
getAllTransactions' Explorer{..} Cache{..} l@Logging{..} TrackerSettings{..} retry = do
  _ <- infoM @String "Going to get next tracker iteration."
  lastIndex <- getLastIndex
  Items{..} <- execWithRetry l retry "Retrying getTxn." (getTxs (Paging lastIndex (naturalToInt limit)) Asc)
  _ <- infoM $ "Got next txn batch: " ++ (show $ length items)
  let 
    cardanoTxn = fmap mkFromExplorer items
    newIndex = fromMaybe lastIndex (fmap (\FullTx{..} -> fromInteger $ unGix globalIndex) (lastMaybe items))
  _ <- infoM $ "New index is: " ++ show newIndex
  
  return (cardanoTxn, newIndex)
