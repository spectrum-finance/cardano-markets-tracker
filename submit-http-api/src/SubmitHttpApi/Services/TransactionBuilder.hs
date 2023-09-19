module SubmitHttpApi.Services.TransactionBuilder where

import qualified Cardano.Api as C
import SubmitHttpApi.Models.TxCreationRequest
import qualified Data.Set as Set
import RIO
import Data.Text as T
import qualified Ledger.Interval     as Interval
import CardanoTx.Models (TxCandidate (..), TxOutCandidate (..), mkPkhTxIn, ChangePolicy (ReturnTo), getAddress, TxOutDatum (EmptyDatum), FullTxOut (..))
import Explorer.Service (Explorer(..))
import Spectrum.Prelude.Throw (throwMaybe, throwEither)
import SubmitHttpApi.Models.TxCreationErrors (TxCreationErrors(CouldNotRetrieveSpfBox, CouldNotParseAddress))
import SubmitHttpApi.Config.AppConfig (AppConfig(..))
import SubmitHttpApi.Models.Common (UserLBSPInfo(..))
import qualified Explorer.Class    as Explorer
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import SubmitAPI.Config (DefaultChangeAddress(getChangeAddr))
import qualified Data.Text.Encoding      as E
import qualified Ledger.Tx.CardanoAPI as Interop
import Plutus.V2.Ledger.Api (Value)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import CardanoTx.Address (readShellyAddress)
import qualified Ledger.Ada                  as Ada
import qualified Ledger                      as P
import qualified Ledger.Value                as P
import qualified PlutusTx.Builtins.Internal  as P
import qualified Ledger.Value                as Value
import Ledger.Value (AssetClass(AssetClass))
import SubmitAPI.Service (Transactions(..))
import Ledger (txId)

data TransactionBuilder m = TransactionBuilder
  { createTx :: TxCreationRequest -> m (C.TxId, Int)
  }

mkTransactionBuilder :: (MonadIO m, MonadThrow m) => AppConfig -> Explorer m -> Transactions m C.BabbageEra -> TransactionBuilder m
mkTransactionBuilder cfg explorer txs =
  TransactionBuilder {
    createTx = createTx' cfg explorer txs
  }

createTx' :: (MonadIO m, MonadThrow m) => AppConfig -> Explorer m -> Transactions m C.BabbageEra -> TxCreationRequest -> m (C.TxId, Int)
createTx' cfg explorer Transactions{..} req@TxCreationRequest{..} = do
  (candidate, spfOutIdx) <- createTxCandidate cfg explorer req
  tx            <- finalizeTx candidate
  transactionId <- submitTx tx
  pure (transactionId, spfOutIdx)

createTxCandidate :: (MonadIO m, MonadThrow m) => AppConfig -> Explorer m -> TxCreationRequest -> m (TxCandidate, Int)
createTxCandidate cfg@AppConfig{..} Explorer{..} req@TxCreationRequest{..} = do
  spfBoxM <- getOutput spfBoxId
  spfBoxE <- throwMaybe (CouldNotRetrieveSpfBox spfBoxId) spfBoxM
  outputs <- mkOutput cfg `traverse` userLBSPInfos
  let
    spfBox      = Explorer.toCardanoTx spfBoxE
    txOutputs   = outputs ++ [produceNexSpfBoxCandidate cfg spfBox req]
    txCandidate = TxCandidate
      { txCandidateInputs       = Set.fromList [mkPkhTxIn spfBox]
      , txCandidateRefIns       = []
      , txCandidateOutputs      = txOutputs
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ getAddress . getChangeAddr $ deafultChangeAddr
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }
  pure (txCandidate, RIO.length txOutputs)

produceNexSpfBoxCandidate :: AppConfig -> FullTxOut -> TxCreationRequest -> TxOutCandidate
produceNexSpfBoxCandidate AppConfig{..} FullTxOut{..} TxCreationRequest{..} =
  let
    spfTN = mkTokenName . mkByteString $ spfTokenName
    spfCS = mkCurrencySymbol . mkByteString $ spfPolicyId
    spfAC = Prelude.foldl (\acc UserLBSPInfo{..} -> acc <> Value.singleton spfCS spfTN (negate spfReward)) (Value.singleton spfCS spfTN 0) userLBSPInfos
    newValue = fullTxOutValue <> spfAC
  in TxOutCandidate
      { txOutCandidateAddress   = fullTxOutAddress
      , txOutCandidateValue     = newValue
      , txOutCandidateDatum     = EmptyDatum
      , txOutCandidateRefScript = Nothing
      }

mkOutput :: (Monad m, MonadThrow m) => AppConfig -> UserLBSPInfo -> m TxOutCandidate
mkOutput cfg@AppConfig{..} info@UserLBSPInfo{..} = do
  rewardAddr <- throwMaybe (CouldNotParseAddress paymentAddress) $ readShellyAddress paymentAddress
  let rewardValue = createValue cfg info
  pure TxOutCandidate
    { txOutCandidateAddress   = rewardAddr
    , txOutCandidateValue     = rewardValue
    , txOutCandidateDatum     = EmptyDatum
    , txOutCandidateRefScript = Nothing
    }

createValue :: AppConfig -> UserLBSPInfo -> Value
createValue AppConfig{..} UserLBSPInfo{..} =
  let
    spfTN = mkTokenName . mkByteString $ spfTokenName
    spfCS = mkCurrencySymbol . mkByteString $ spfPolicyId
    spfAC = Value.singleton spfCS spfTN spfReward
    adaAC = Value.singleton Ada.adaSymbol Ada.adaToken providedAda
  in spfAC <> adaAC

mkByteString :: T.Text -> BS.ByteString
mkByteString input = unsafeFromEither (Hex.decode . E.encodeUtf8 $ input)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

mkTokenName :: BS.ByteString -> P.TokenName
mkTokenName = P.TokenName . P.BuiltinByteString

mkCurrencySymbol :: BS.ByteString -> P.CurrencySymbol
mkCurrencySymbol = P.CurrencySymbol . P.BuiltinByteString

mkAssetClass :: BS.ByteString -> BS.ByteString -> P.AssetClass
mkAssetClass cs tn = P.AssetClass (mkCurrencySymbol cs, mkTokenName tn)