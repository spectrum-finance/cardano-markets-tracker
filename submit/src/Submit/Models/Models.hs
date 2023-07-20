module Submit.Models.Models
    ( PolicyId(..)
    , AssetName(..)
    , OutAsset(..)
    , outAssetToValue
    , ApiInput(..)
    , ApiOutput(..)
    , EncryptedSignKey(..)
    , DecryptedSignKey(..)
    ) where


import           RIO
import qualified Data.Text as T

import           Ledger as L (TxOutRef, PubKeyHash(..), Value)
import qualified Plutus.V1.Ledger.Value            as Value

newtype PolicyId = PolicyId { unPolicyId :: Text }

newtype AssetName = AssetName { unAssetName :: Text }

data OutAsset = OutAsset
  { policy      :: PolicyId
  , name        :: AssetName
  , quantity    :: Integer
  }

outAssetToValue :: OutAsset -> L.Value
outAssetToValue OutAsset{..} = Value.singleton p n quantity
    where
      p = fromString $ T.unpack $ unPolicyId policy
      n = fromString $ T.unpack $ unAssetName name

data ApiInput = ApiInput
    { boxRef     :: TxOutRef
    , inputPkh   :: PubKeyHash
    , inputValue :: [OutAsset]
    }

data ApiOutput = ApiOutput
    { outputPkh   :: PubKeyHash
    , outputValue :: [OutAsset]
    }

newtype EncryptedSignKey = EncryptedSignKey { unEncryptedKey :: Text }

newtype DecryptedSignKey = DecryptedSignKey { unDecryptedKey :: Text }