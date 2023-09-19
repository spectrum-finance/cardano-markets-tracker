module SubmitHttpApi.Models.TxCreationErrors where

import Ledger (TxOutRef)
import RIO

data TxCreationErrors =
    CouldNotRetrieveSpfBox TxOutRef |
    CouldNotParseAddress Text
    deriving (Generic, Show)

instance Exception TxCreationErrors