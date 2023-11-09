module SubmitHttpApi.Config.AppConfig where

import Spectrum.LedgerSync.Config (NodeSocketConfig)
import Dhall
  ( Generic, Text, FromDhall, auto, input, Natural )
import WalletAPI.TrustStore 
  ( SecretFile, KeyPass )
import RIO (MonadIO, liftIO, some)
import qualified Data.Text as T
import RIO.Prelude (fromMaybe)
import System.Logging.Hlog (LoggingConfig)
import SubmitAPI.Config
import NetworkAPI.HttpService (HttpServiceConfig)
import Cardano.Api (TxIn(..), TxId(..), TxIx(..), deserialiseFromRawBytesHex, AsType (AsTxId), Error (displayError))
import Dhall.Core
  ( Expr(..), Chunks(..) )
import qualified Dhall     as D
import qualified Data.Text as T
import Text.Parsec
import qualified Plutus.V2.Ledger.Api as PV2
import Text.Parsec 
  ( (<?>) )
import qualified Text.Parsec          as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.String   as Parsec
import qualified Text.Parsec.Token    as Parsec

import qualified Data.ByteString.Char8 as BSC
import Explorer.Config (ExplorerConfig)

data HttpConfig = HttpConfig
  { host :: String
  , port :: Natural 
  } deriving (Generic, FromDhall)

data Secrets = Secrets
  { secretFile :: !SecretFile
  , keyPass    :: !KeyPass
  } deriving (Generic, FromDhall)

data AppConfig = AppConfig
  { nodeSocketConfig    :: !NodeSocketConfig
  , httpConfig          :: !HttpConfig
  , secrets             :: !Secrets
  , loggingConfig       :: !LoggingConfig
  , explorerConfig      :: !ExplorerConfig
  , deafultChangeAddr   :: !DefaultChangeAddress
  , txAssemblyConfig    :: !TxAssemblyConfig
  , spfPolicyId         :: !Text
  , spfTokenName        :: !Text
  , unsafeEval          :: !UnsafeEvalConfig
  , httpSubmit          :: !HttpServiceConfig
  } deriving (Generic)

instance FromDhall AppConfig

loadAppConfig :: MonadIO f => Maybe String -> f AppConfig
loadAppConfig maybePath = liftIO $ input auto path
  where path = T.pack $ fromMaybe "./submit-http-api/resources/config.dhall" maybePath