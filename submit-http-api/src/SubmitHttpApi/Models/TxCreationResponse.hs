module SubmitHttpApi.Models.TxCreationResponse where

import GHC.Generics
  ( Generic )
import Data.Aeson 
  ( FromJSON, ToJSON )

data TxCreationResponse = TxCreationSuccessResponse
  { txId     :: String 
  , spfBoxId :: Integer
  } | TxFailureResponse {
    error :: String
  } 
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

-- data TxFailureResponse = TxFailureResponse String 

-- data TxCreationSuccessResponse = TxCreationSuccessResponse
--   { txId     :: String 
--   , spfBoxId :: Integer
--   } deriving (Generic, Eq, Show, FromJSON, ToJSON)