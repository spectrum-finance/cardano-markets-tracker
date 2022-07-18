module Tracker.Programs.TrackerProgram where

import RIO hiding 
  ( elem )
import System.Time.Extra 
  ( sleep )
import qualified Streamly.Prelude as S

import Streaming.Producer 
  ( Producer(produce) )
import System.Logging.Hlog 
  ( Logging(..), MakeLogging(..) )

import ErgoDex.Class    
  ( FromLedger(parseFromLedger) )
import ErgoDex.Amm.Pool 
  ( Pool(..) )
import ErgoDex.State    
  ( OnChain(OnChain) )
import Explorer.Class   
  ( FromExplorer(..) )
import CardanoTx.Models 
  ( FullTxOut(fullTxOutRef) )

import Tracker.Services.Tracker 
  ( TrackerService(..) )
import Tracker.Caches.Cache     
  ( Cache(..) )
import Tracker.Models.AppConfig 
  ( TrackerProgrammConfig(..) )
import Tracker.Syntax.Option    
  ( unNone )
import Tracker.Models.ExecutedOrders
  ( ExecutedRedeem, ExecutedDeposit, ExecutedSwap )
import Tracker.Models.SettledTx
  ( SettledTx(SettledTx, outputs, timestamp) )
import Tracker.Models.Events.PoolEvent 
  ( PoolEvent(PoolEvent) )
import Tracker.Models.Interop.Class 
  ( CardanoWrapper(wrap) )
import Tracker.Models.Events.AnyEOrder 
  ( AnyEOrder )
import Tracker.Models.Events.Class 
  ( MakeAnyOrder(..) )
import qualified Tracker.Models.Interop.Pool as Interop

data TrackerProgram f = TrackerProgram
  { run :: f ()
  }

mkTrackerProgram
  :: (Monad i, MonadUnliftIO f)
  => TrackerProgrammConfig
  -> MakeLogging i f
  -> Cache f
  -> TrackerService f
  -> Producer f String AnyEOrder
  -> Producer f String PoolEvent
  -> i (TrackerProgram f)
mkTrackerProgram settings MakeLogging{..} cache tracker executedOrdersProducer poolsProducer = do
  logger <- forComponent "trackerProgram"
  pure $ TrackerProgram $ run' settings logger cache tracker executedOrdersProducer poolsProducer

run'
  :: (MonadUnliftIO f)
  => TrackerProgrammConfig
  -> Logging f
  -> Cache f
  -> TrackerService f
  -> Producer f String AnyEOrder
  -> Producer f String PoolEvent
  -> f ()
run' config logging@Logging{..} cache service executedOrdersProducer poolsProducer =
    handle
      (\(a :: SomeException) -> errorM ("tracke error: " ++ show a))
      (process config service cache logging executedOrdersProducer poolsProducer)

process
  :: (MonadIO f)
  => TrackerProgrammConfig
  -> TrackerService f
  -> Cache f
  -> Logging f
  -> Producer f String AnyEOrder
  -> Producer f String PoolEvent
  -> f ()
process cfg@TrackerProgrammConfig{..} service@TrackerService{..} cache@Cache{..} logging@Logging{..} executedOrdersProducer poolsProducer = do
  getAllTransactions >>=
    (\(transactions, newIndex, maxIndex) -> do
      processTxBatch transactions logging executedOrdersProducer poolsProducer
      putLastIndex newIndex
      if newIndex == maxIndex
        then
          let sleepTime = fromIntegral pollTime
          in debugM ("Reached max index. Sleep for " ++ show sleepTime) >> liftIO (sleep sleepTime)
        else debugM @String "Max index wasn't reach. Going to get next batch"
      process cfg service cache logging executedOrdersProducer poolsProducer
    )

processTxBatch
  :: (Monad f)
  => [SettledTx]
  -> Logging f
  -> Producer f String AnyEOrder
  -> Producer f String PoolEvent
  -> f ()
processTxBatch transactions Logging{..} executedOrdersProducer poolsProducer = do
  let
    events =
        executedSwaps ++ executedDeposits ++ executedRedeems
      where
        executedSwaps    = processExecutedOrder @ExecutedSwap transactions
        executedDeposits = processExecutedOrder @ExecutedDeposit transactions
        executedRedeems  = processExecutedOrder @ExecutedRedeem transactions
    pools = processPool transactions
  infoM $ "Events are: "  ++ show (length events)
  unless (null events) (produce executedOrdersProducer (S.fromList events))
  infoM $ "Pools are: "  ++ show (length pools)
  unless (null pools) (produce poolsProducer (S.fromList pools))

constantKafkaKey :: String
constantKafkaKey = "kafka_key"

processExecutedOrder :: forall b. (FromExplorer SettledTx b, MakeAnyOrder b AnyEOrder) => [SettledTx] -> [(String, AnyEOrder)]
processExecutedOrder inputs =
  let
    ordersMaybe =
      fmap (\elem ->
        case parseFromExplorer elem :: Maybe b of
          Just order -> Just (constantKafkaKey, make order)
          _          -> Nothing
        ) inputs
  in unNone ordersMaybe

processPool :: [SettledTx] -> [(String, PoolEvent)]
processPool transactions =
  let
    poolsMaybe = transactions >>= (\SettledTx{outputs, timestamp} ->
        fmap (\out -> case parseFromLedger @Pool out of
            Just (OnChain out Pool{..}) ->
              let
                pool = Interop.Pool
                  { id            = wrap poolId
                  , reservesX     = poolReservesX
                  , reservesY     = poolReservesY
                  , liquidity     = poolLiquidity
                  , x             = wrap poolCoinX
                  , y             = wrap poolCoinY
                  , lq            = wrap poolCoinLq
                  , fee           = wrap poolFee
                  , outCollateral = outCollateral
                  }
              in Just (show poolId, PoolEvent pool timestamp (fullTxOutRef out))
            _ -> Nothing
          ) outputs
      )
  in unNone poolsMaybe
