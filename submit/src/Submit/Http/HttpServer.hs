{-# LANGUAGE TypeOperators #-}

module Submit.Http.HttpServer
  ( HttpServer(..)
  ) where

import Submit.Services.TxMaker
import Submit.Models.Models
import Submit.Services.Encryption 

import           RIO
import qualified RIO.List as List
import System.Logging.Hlog (MakeLogging(..))

import Explorer.Service
import WalletAPI.UtxoStoreConfig

import Data.Int
import Control.Monad.Trans.Except
import Servant
import Network.Wai.Handler.Warp as Warp
import GHC.Natural


data HttpServer f = HttpServer
  { runHttpServer :: f ()
  }
 -- ApiTransaction, ApiError
type Api =
  "finalize" :> ReqBody '[JSON] ([ApiInput], [ApiOutput], EncryptedSignKey) :> Post '[JSON] () :<|>
  "submit"   :> ReqBody '[JSON] ()                              :> Post '[JSON] (Maybe ())

finalize 
    :: CardanoNetwork f C.BabbageEra
    -> C.NetworkId
    -> TxAssemblyConfig
    -> UtxoStoreConfig
    -> MakeLogging i f
    -> Explorer f
    -> Encryption 
    -> [ApiInput] 
    -> [ApiOutput] 
    -> EncryptedSignKey 
    -> f ()
finalize network networkId txAssemblyConfig mkLogging utxoStoreConfig explorer Encryption{..} inputs outputs encryptedSignKey = do
    signKey         <- decrypt encryptedSignKey
    walletOutputs   <- mkUtxoStore mkLogging
    let 
        transactions = mkTransactions network networkId mempty walletOutputs (mkVault signKey) txAssemblyConfig
        inputs = List.map mkFullTxIn inputs
        outs = List.map mkTxOutCandidate outputs 
        pkh = List.map mkPkh inputs
        tx = mkTxCandidate pkh inputs outs
    return tx

