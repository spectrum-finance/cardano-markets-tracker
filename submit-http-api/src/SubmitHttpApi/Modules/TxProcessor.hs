module SubmitHttpApi.Modules.TxProcessor where
import Cardano.Api (TxId)
import SubmitHttpApi.Models.TxCreationRequest (TxCreationRequest(..))
import SubmitHttpApi.Models.TxCreationResponse (TxCreationResponse(..))
import System.Logging.Hlog (Logging(..), MakeLogging (..), makeLogging)
import SubmitHttpApi.Services.TransactionBuilder (TransactionBuilder(..))
import Data.Functor ((<&>))
import RIO (handle, MonadUnliftIO, SomeException)
import RIO.Prelude.Types (MonadThrow)

data TxProcessor m = TxProcessor
  { processCreationRequest :: TxCreationRequest -> m TxCreationResponse
  }

mkTxProcessor :: (Monad m, MonadThrow m, MonadUnliftIO m) => MakeLogging m m -> TransactionBuilder m -> m (TxProcessor m)
mkTxProcessor MakeLogging{..} TransactionBuilder{..} = do
  logging <- forComponent "TxProcessor"
  pure $ attachLogging logging TxProcessor 
    { processCreationRequest = \req -> handle (\(e :: SomeException) -> pure $ TxFailureResponse (show e)) (createTx req <&> (\(txId, spfBoxIdx) -> TxCreationSuccessResponse (show txId) (fromIntegral spfBoxIdx)))
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