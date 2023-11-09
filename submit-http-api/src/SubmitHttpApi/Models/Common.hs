module SubmitHttpApi.Models.Common where

import GHC.Generics
  ( Generic )
import Data.Aeson 
  ( FromJSON, ToJSON )
import Data.Text (Text)

data UserLBSPInfo = UserLBSPInfo
  { paymentAddress :: Text
  , spfReward      :: Integer
  , providedAda    :: Integer
  } deriving (Generic, Eq, Show, FromJSON, ToJSON)