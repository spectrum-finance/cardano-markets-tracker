module SubmitHttpApi.Models.TxCreationRequest where

import Ledger
  ( TxOutRef )

import GHC.Generics
  ( Generic )
import Data.Aeson 
  ( FromJSON, ToJSON )

import SubmitHttpApi.Models.Common

data TxCreationRequest = TxCreationRequest
  { spfBoxId      :: TxOutRef
  , userLBSPInfos :: [UserLBSPInfo]
  } deriving (Generic, Eq, Show, FromJSON, ToJSON)