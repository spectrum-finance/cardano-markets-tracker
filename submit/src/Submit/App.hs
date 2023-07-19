import SubmitAPI.Service
import System.Logging.Hlog
  ( makeLogging, MakeLogging (forComponent), translateMakeLogging, Logging (infoM) )
import WalletAPI.Utxos
  ( mkPersistentWalletOutputs )
import Explorer.Service
  ( mkExplorer )

run :: ()
run = do
    -- app config
    -- loggingConfig, mainnetMode, nodeSocketConfig, nodeSocketPath, utxoStoreConfig, explorerConfig, Secrets (secretFile), txAssemblyConfig
    let
        networkId =
            if mainnetMode
            then C.Mainnet
            else C.Testnet (C.NetworkMagic (fromIntegral $ cardanoNetworkId networkConfig))
    let sockPath = SocketPath $ nodeSocketPath nodeSocketConfig
    let
        trustStore = mkTrustStore @_ @C.PaymentKey C.AsPaymentKey (secretFile secrets)
        vault      = mkVault trustStore $ keyPass secrets
    mkLogging      <- makeLogging loggingConfig
    networkService <- mkCardanoNetwork mkLogging C.BabbageEra epochSlots networkId sockPath
    explorer       <- mkExplorer mkLogging explorerConfig
    walletOutputs  <- mkPersistentWalletOutputs lift mkLogging utxoStoreConfig explorer vault
    let transactions = mkTransactions networkService networkId Map.empty walletOutputs vault txAssemblyConfig

epochSlots :: C.ConsensusModeParams C.CardanoMode
epochSlots = C.CardanoModeParams $ C.EpochSlots 21600