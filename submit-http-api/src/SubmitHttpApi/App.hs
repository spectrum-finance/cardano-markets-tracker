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
import WalletAPI.Vault (Vault(..), mkVault)
import WalletAPI.TrustStore (mkTrustStore)
import WalletAPI.Utxos
import Explorer.Service (mkExplorer)
import SubmitAPI.Service (mkTransactions)
import SubmitHttpApi.Services.TransactionBuilder (mkTransactionBuilder)

runApp :: [String] -> IO ()
runApp args = do
  cfg@AppConfig{..} <- loadAppConfig (headMaybe args)
  mkLogging     <- makeLogging loggingConfig :: IO (MakeLogging IO IO)
  transactionsLogging <- forComponent mkLogging "Bots.Transactions"
  explorer <- mkExplorer mkLogging explorerConfig
  let
    sockPath = SocketPath $ nodeSocketPath nodeSocketConfig
    trustStore = mkTrustStore @_ @C.PaymentKey C.AsPaymentKey (secretFile secrets)
    vault      = mkVault trustStore $ keyPass secrets :: Vault IO
  pkh <- getPaymentKeyHash vault
  walletOutputs <- mkWalletOutputs mkLogging explorer pkh
  httpSubmitNetwork <- mkHttpCardanoNetwork mkLogging httpSubmit
  networkService <- mkCardanoNetwork mkLogging C.BabbageEra epochSlots C.Mainnet sockPath
  let
    transactions = mkTransactions unsafeEval transactionsLogging networkService httpSubmitNetwork C.Mainnet Map.empty walletOutputs vault txAssemblyConfig
    txBuilder    = mkTransactionBuilder cfg explorer transactions
  txProcessor   <- mkTxProcessor mkLogging txBuilder
  runHttpServer txProcessor httpConfig

epochSlots :: C.ConsensusModeParams C.CardanoMode
epochSlots = C.CardanoModeParams $ C.EpochSlots 21600