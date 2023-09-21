module SubmitHttpApi.App where

import SubmitHttpApi.Config.AppConfig (AppConfig(..), loadAppConfig, Secrets (secretFile, keyPass))
import RIO.List (headMaybe)
import System.Logging.Hlog (MakeLogging (forComponent), makeLogging)
import SubmitHttpApi.Modules.TxProcessor (mkTxProcessor)
import SubmitHttpApi.Http.Server ( runHttpServer )
import qualified Cardano.Api as C
import NetworkAPI.Service (mkCardanoNetwork)
import NetworkAPI.Types
import Spectrum.LedgerSync.Config (nodeSocketPath)
import NetworkAPI.HttpService (mkHttpCardanoNetwork)
import qualified Data.Map as Map
import qualified Data.Text as Text
import WalletAPI.Vault (Vault(..), mkVault)
import WalletAPI.TrustStore (mkTrustStore, KeyPass (..))
import WalletAPI.Utxos
import Explorer.Service (mkExplorer)
import SubmitAPI.Service (mkTransactions)
import SubmitHttpApi.Services.TransactionBuilder (mkTransactionBuilder)
import System.Console.Haskeline
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))

runApp :: [String] -> IO ()
runApp args = do
  cfg@AppConfig{..} <- loadAppConfig (headMaybe args)
  pass <- runInputT defaultSettings $ getPassword (Just '*') "Password: "
  mkLogging     <- makeLogging loggingConfig :: IO (MakeLogging IO IO)
  transactionsLogging <- forComponent mkLogging "Bots.Transactions"
  explorer <- mkExplorer mkLogging explorerConfig
  let
    sockPath = SocketPath $ nodeSocketPath nodeSocketConfig
    trustStore = mkTrustStore @_ @C.PaymentKey C.AsPaymentKey (secretFile secrets)
    vault      = mkVault trustStore $ fromMaybe (keyPass secrets) (pass <&> (KeyPass . Text.pack)) :: Vault IO
  pkh <- getPaymentKeyHash vault
  walletOutputs <- mkWalletOutputs mkLogging explorer pkh
  httpSubmitNetwork <- mkHttpCardanoNetwork mkLogging httpSubmit
  networkService <- mkCardanoNetwork mkLogging C.BabbageEra epochSlots C.Mainnet sockPath
  let
    transactions = mkTransactions unsafeEval transactionsLogging networkService httpSubmitNetwork C.Mainnet Map.empty walletOutputs vault txAssemblyConfig
    txBuilder    = mkTransactionBuilder transactionsLogging cfg explorer transactions walletOutputs
  txProcessor   <- mkTxProcessor mkLogging txBuilder
  runHttpServer txProcessor httpConfig

epochSlots :: C.ConsensusModeParams C.CardanoMode
epochSlots = C.CardanoModeParams $ C.EpochSlots 21600