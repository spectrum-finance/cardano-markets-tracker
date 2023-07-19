module Submit.Models.Models
    ( TokenId(..)
    , ApiInput(..)
    , ApiOutput(..)
    ) where

newtype TokenId = TokenId { unTokenId :: Text }

data ApiInput = ApiInput
    { boxRef  :: TxOutRef
    , address :: Address
    , value   :: [(TokenId, Integer)]
    }

data ApiOutput = ApiOutput
    { address :: Address
    , value   :: [(TokenId, Integer)]
    }