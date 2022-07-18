module Tracker.Services.Tracker where

import RIO
import RIO.List 
  ( lastMaybe )
import GHC.Natural
  ( naturalToInt )
import Control.Monad.Catch 
  ( MonadMask )

import Explorer.Service
  ( Explorer(..) )
import Explorer.Models
  ( FullTx(FullTx, timestamp, outputs, inputs, hash, globalIndex, blockIndex, blockHash)
  , Items(Items, total, items)
  , Paging(Paging) 
  )
import Explorer.Types 
  ( Gix(unGix), Ordering(Asc) )
import System.Logging.Hlog 
  ( Logging(..), MakeLogging(..) )

import Tracker.Syntax.Retry 
  ( execWithRetry )
import Tracker.Caches.Cache 
  ( Cache(..) )
import Tracker.Models.AppConfig
  ( RetryConfig, TrackerSettings(..) )
import Tracker.Models.SettledTx 
  ( SettledTx, mkFromExplorer )

data TrackerService f = TrackerService
  { getAllTransactions :: f ([SettledTx], Int, Int)
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
  -> f ([SettledTx], Int, Int)
getAllTransactions' Explorer{..} Cache{..} l@Logging{..} TrackerSettings{..} retry = do
  infoM @String "Going to get next tracker iteration."
  lastIndex <- getLastIndex
  Items{..} <- execWithRetry l retry "Retrying getTxn." (getTxs (Paging lastIndex (naturalToInt limit)) Asc)
  infoM $ "Got next txn batch: " ++ show (length items)
  let
    cardanoTxn = fmap mkFromExplorer items
    newIndex = maybe lastIndex (\FullTx{..} -> fromInteger (unGix globalIndex) + 1) (lastMaybe items)
  infoM $ "New index is: " ++ show newIndex

  return (cardanoTxn, newIndex, total)
