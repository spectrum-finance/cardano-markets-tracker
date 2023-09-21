module SubmitHttpApi.Models.TxCreationErrors where

import Ledger (TxOutRef)
import RIO

data TxCreationErrors =
    CouldNotRetrieveSpfBox TxOutRef |
    CouldNotParseAddress Text |
    CouldNotCollectInputsToCoverAdaBalance
    deriving (Generic, Show)

instance Exception TxCreationErrors