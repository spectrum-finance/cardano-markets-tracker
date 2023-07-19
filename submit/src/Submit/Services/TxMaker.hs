module Submit.Services.TxMaker 
    ( mkPkh
    , mkFullTxIn
    , mkTxOutCandidate
    , mkTxCandidate
    ) where

import Submit.Models.Models

import CardanoTx.Models

import           Ledger (PaymentPubKeyHash(..), pubKeyHashAddress)
import qualified Ledger.Interval as Interval

mkPkh :: Address -> PubKeyHash

mkFullTxIn :: ApiInput -> FullTxIn
mkFullTxIn ApiInput{..} =
    let fullTxOut = 
        FullTxOut
            { fullTxOutRef       = boxRef
            , fullTxOutAddress   = address
            , fullTxOutValue     = value
            , fullTxOutDatum     = EmptyDatum
            , fullTxOutScriptRef = Nothing
            }
    return $ mkPkhTxIn fullTxOut

mkTxOutCandidate :: ApiOutput -> TxOutCandidate
mkTxOutCandidate ApiOutput{..} =
    TxOutCandidate 
        { txOutCandidateAddress   = address
        , txOutCandidateValue     = value
        , txOutCandidateDatum     = EmptyDatum
        , txOutCandidateRefScript = Nothing
        } 

mkTxCandidate :: PubKeyHash -> [FullTxIn] -> [TxOutCandidate] -> TxCandidate
mkTxCandidate pkh inputs outputs =
    let rewardAddr = pubKeyHashAddress (PaymentPubKeyHash pkh) Nothing
    TxCandidate
        { txCandidateInputs       = inputs
        , txCandidateRefIns       = List.fmap (\FullTxIn{..} -> fullTxInTxOut) inputs
        , txCandidateOutputs      = outputs
        , txCandidateValueMint    = mempty
        , txCandidateMintInputs   = mempty
        , txCandidateChangePolicy = Just . ReturnTo $ rewardAddr
        , txCandidateValidRange   = Interval.always
        , txCandidateSigners      = mempty
        }
