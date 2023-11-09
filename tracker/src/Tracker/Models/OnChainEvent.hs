module Tracker.Models.OnChainEvent where
    
import RIO 
  ( Generic )
import Data.Aeson 
  ( FromJSON, ToJSON )
import Data.Text
import Cardano.Api 
  ( SlotNo )

import ErgoDex.State 
  ( OnChain )

data OnChainEvent a = OnChainEvent
  { event  :: OnChain a
  , slotNo :: SlotNo
  } deriving (Show, Eq, Generic, FromJSON, ToJSON) 