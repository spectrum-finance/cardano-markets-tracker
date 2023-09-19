module SubmitHttpApi.Models.TxCreationResponse where

import GHC.Generics
  ( Generic )
import Data.Aeson 
  ( FromJSON, ToJSON )

data TxCreationResponse = TxCreationResponse
  { txId     :: String 
  , spfBoxId :: Integer
  } deriving (Generic, Eq, Show, FromJSON, ToJSON)