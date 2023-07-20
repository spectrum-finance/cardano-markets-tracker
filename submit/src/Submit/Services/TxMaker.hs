module Submit.Services.TxMaker 
    ( mkFullTxIn
    , mkTxOutCandidate
    , mkTxCandidate
    ) where

import Submit.Models.Models

import CardanoTx.Models

import           Ledger (PaymentPubKeyHash(..), pubKeyHashAddress, PubKeyHash(..))
import qualified Ledger.Interval as Interval

import qualified RIO.List as List
import qualified RIO.Set  as Set

mkFullTxIn :: ApiInput -> FullTxIn
mkFullTxIn ApiInput{..} = do
    mkPkhTxIn $ FullTxOut
        { fullTxOutRef       = boxRef
        , fullTxOutAddress   = pubKeyHashAddress (PaymentPubKeyHash inputPkh) Nothing
        , fullTxOutValue     = List.foldl (<>) mempty (List.map outAssetToValue inputValue)
        , fullTxOutDatum     = EmptyDatum
        , fullTxOutScriptRef = Nothing
        }

mkTxOutCandidate :: ApiOutput -> TxOutCandidate
mkTxOutCandidate ApiOutput{..} = do
    TxOutCandidate 
        { txOutCandidateAddress   = pubKeyHashAddress (PaymentPubKeyHash outputPkh) Nothing
        , txOutCandidateValue     = List.foldl (<>) mempty (List.map outAssetToValue outputValue)
        , txOutCandidateDatum     = EmptyDatum
        , txOutCandidateRefScript = Nothing
        } 

mkTxCandidate :: PubKeyHash -> [FullTxIn] -> [TxOutCandidate] -> TxCandidate
mkTxCandidate pkh inputs outputs = do
    let rewardAddr = pubKeyHashAddress (PaymentPubKeyHash pkh) Nothing
    TxCandidate
        { txCandidateInputs       = Set.fromList inputs
        , txCandidateRefIns       = List.map (\FullTxIn{..} -> fullTxInTxOut) inputs
        , txCandidateOutputs      = outputs
        , txCandidateValueMint    = mempty
        , txCandidateMintInputs   = mempty
        , txCandidateChangePolicy = Just . ReturnTo $ rewardAddr
        , txCandidateValidRange   = Interval.always
        , txCandidateSigners      = mempty
        }
