module SubmitHttpApi.Modules.TxProcessor where
import Cardano.Api (TxId)
import SubmitHttpApi.Models.TxCreationRequest (TxCreationRequest(..))
import SubmitHttpApi.Models.TxCreationResponse (TxCreationResponse(..))
import System.Logging.Hlog (Logging(..), MakeLogging (..), makeLogging)
import SubmitHttpApi.Services.TransactionBuilder (TransactionBuilder(..))
import Data.Functor ((<&>))

data TxProcessor m = TxProcessor
  { processCreationRequest :: TxCreationRequest -> m TxCreationResponse
  }

mkTxProcessor :: (Monad m) => MakeLogging m m -> TransactionBuilder m -> m (TxProcessor m)
mkTxProcessor MakeLogging{..} TransactionBuilder{..} = do
  logging <- forComponent "TxProcessor"
  pure $ attachLogging logging TxProcessor 
    { processCreationRequest = \req -> createTx req <&> (\(txId, spfBoxIdx) -> TxCreationResponse (show txId) (fromIntegral spfBoxIdx))
    }

attachLogging :: Monad m => Logging m -> TxProcessor m -> TxProcessor m
attachLogging Logging{..} TxProcessor{..} =
  TxProcessor
    { processCreationRequest = \req -> do
        infoM $ "processCreationRequest: " <> show req
        r <- processCreationRequest req
        infoM $ "processCreationRequest " <> show req <> " -> " <> show r
        pure r
    }