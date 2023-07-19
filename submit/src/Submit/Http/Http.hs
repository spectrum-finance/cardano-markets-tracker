{-# LANGUAGE TypeOperators #-}

module Resolver.Endpoints.HttpServer
  ( HttpServer(..)
  , mkHttpServer
  ) where

import Submit.Services.TxMaker
import Submit.Models.Models

import Resolver.Models.AppSettings as AppSettings
import Resolver.Repositories.PoolRepository
import Resolver.Services.PoolResolver
import RIO
import Data.Int
import Control.Monad.Trans.Except
import Servant
import Network.Wai.Handler.Warp as Warp
import GHC.Natural
import ErgoDex.Amm.Pool
import Cardano.Models
import Explorer.Types
import Resolver.Models.Types

data HttpServer f = HttpServer
  { runHttpServer :: f ()
  }

type Api =
  "finalize" :> ReqBody '[JSON] ([ApiInput], [ApiOutput], EncryptedSignKey) :> Post '[JSON] ApiTransaction :<|>
  "submit"   :> ReqBody '[JSON] ApiTransaction                              :> Post '[JSON] (Maybe ApiError)

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
    -> f ApiTransaction
finalize network networkId txAssemblyConfig mkLogging utxoStoreConfig explorer Encryption{..} inputs outputs encryptedSignKey = do
    signKey         <- decrypt encryptedSignKey
    walletOutputs   <- mkUtxoStore mkLogging
    let 
        transactions = mkTransactions network networkId Map.empty walletOutputs (mkVault signKey) txAssemblyConfig
        inputs = List.fmap inputs mkFullTxIn
        outs = List.fmap outputs mkTxOutCandidate
        pkh = List.fmap inputs mkPkh
        tx = mkTxCandidate pkh inputs outs
    return tx

