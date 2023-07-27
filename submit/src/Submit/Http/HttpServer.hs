{-# LANGUAGE TypeOperators #-}

module Submit.Http.HttpServer
  ( HttpServer(..)
  ) where

import Submit.Services.TxMaker
import Submit.Models.Models
import Submit.Services.Encryption 
import Submit.Http.Models

import           RIO
import qualified RIO.List as List
import System.Logging.Hlog (MakeLogging(..))
import qualified Cardano.Api.Shelley         as C

import Explorer.Service
import WalletAPI.UtxoStoreConfig
import WalletAPI.UtxoStore
import WalletAPI.Utxos
import NetworkAPI.Service
import SubmitAPI.Config
import SubmitAPI.Service
import WalletAPI.Vault as Vault

import Algebra.Natural
import Control.Monad.Trans.Except
import Control.Monad.Catch
import Servant
import Data.Aeson.Types ( ToJSON(..), FromJSON )

data HttpServer f = HttpServer
  { runHttpServer :: f ()
  }
 -- ApiTransaction, ApiError
type SubmitAPI =
  "finalize" :> ReqBody '[JSON] ([ApiInput], [ApiOutput], EncryptedSignKey) :> Post '[JSON] SubmitApiResponse :<|>
  "submit"   :> ReqBody '[JSON] ApiInput                              :> Post '[JSON] SubmitApiResponse

submitAPI :: Proxy SubmitAPI
submitAPI = Proxy

f2Handler :: (MonadIO f) => UnliftIO f -> f a -> Servant.Handler a
f2Handler UnliftIO{..} = liftIO . unliftIO

httpApp  
    :: forall i f. (MonadIO i, MonadIO f, MonadMask f, MonadMask i, MonadThrow f) 
    => (f ~> i)
    -> (i ~> f)
    -> CardanoNetwork f C.BabbageEra
    -> C.NetworkId
    -> TxAssemblyConfig
    -> UtxoStoreConfig
    -> MakeLogging i f
    -> Explorer f
    -> Encryption f 
    -> UnliftIO f 
    -> Application
httpApp xa xs network networkId txAssemblyConfig utxoStoreConfig mkLogging explorer encryption un = 
  serve submitAPI $ hoistServer submitAPI (f2Handler un) (submitApp xa xs network networkId txAssemblyConfig utxoStoreConfig mkLogging explorer encryption)

submitApp 
    :: forall i f. (MonadIO i, MonadIO f, MonadMask f, MonadMask i, MonadThrow f) 
    => (f ~> i)
    -> (i ~> f)
    -> CardanoNetwork f C.BabbageEra
    -> C.NetworkId
    -> TxAssemblyConfig
    -> UtxoStoreConfig
    -> MakeLogging i f
    -> Explorer f
    -> Encryption f
    -> ServerT SubmitAPI f
submitApp xa xs network networkId txAssemblyConfig utxoStoreConfig mkLogging explorer encryption =
  finalize xa xs network networkId txAssemblyConfig utxoStoreConfig mkLogging explorer encryption :<|>
  submit

submit :: ApiInput -> f SubmitApiResponse
submit _ = undefined

finalize
    :: forall i f. (MonadIO i, MonadIO f, MonadMask f, MonadMask i, MonadThrow f) 
    => (f ~> i)
    -> (i ~> f)
    -> CardanoNetwork f C.BabbageEra
    -> C.NetworkId
    -> TxAssemblyConfig
    -> UtxoStoreConfig
    -> MakeLogging i f
    -> Explorer f
    -> Encryption f
    -> [ApiInput] 
    -> [ApiOutput] 
    -> EncryptedSignKey 
    -> f SubmitApiResponse
finalize xa xs network networkId txAssemblyConfig utxoStoreConfig mkLogging explorer encryption =
  catchAll 
    (finalize' xa xs network networkId txAssemblyConfig utxoStoreConfig mkLogging explorer encryption) 
    (\err -> err' err)

err' :: (Typeable e, Show e) => e -> [ApiInput] -> [ApiOutput] -> EncryptedSignKey -> f SubmitApiResponse
err' err _ _ _ = pure TxError

finalize'
    :: forall i f. (MonadIO i, MonadIO f, MonadMask f, MonadMask i, MonadThrow f) 
    => (f ~> i)
    -> (i ~> f)
    -> CardanoNetwork f C.BabbageEra
    -> C.NetworkId
    -> TxAssemblyConfig
    -> UtxoStoreConfig
    -> MakeLogging i f
    -> Explorer f
    -> Encryption f
    -> [ApiInput] 
    -> [ApiOutput] 
    -> EncryptedSignKey 
    -> f SubmitApiResponse
finalize' xa xs network networkId txAssemblyConfig utxoStoreConfig mkLogging explorer Encryption{..} inputs outputs encryptedSignKey = do
    signKey       <- decrypt encryptedSignKey
    let vault = mkVault' signKey
    walletOutputs <- xs $ mkWalletOutputs' xa mkLogging explorer vault
    let 
        Transactions{..} = mkTransactions network networkId mempty walletOutputs vault txAssemblyConfig
        fullTxinputs = List.map mkFullTxIn inputs
        outs         = List.map mkTxOutCandidate outputs 
        pkh          = inputPkh . fromJust $ List.headMaybe inputs
        tx           = mkTxCandidate pkh fullTxinputs outs
    res <- finalizeTx tx
    return Tx

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Oops, you goofed up, fool."

mkVault' :: DecryptedSignKey -> Vault.Vault f
mkVault' _ = undefined