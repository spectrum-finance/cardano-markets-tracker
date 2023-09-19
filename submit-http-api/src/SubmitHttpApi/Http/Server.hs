{-# LANGUAGE TypeOperators #-}

module SubmitHttpApi.Http.Server where

import SubmitHttpApi.Models.TxCreationRequest
import SubmitHttpApi.Models.TxCreationResponse
import Servant.API ( type (:>), ReqBody, JSON, Post )
import Servant
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson.Types
import RIO (liftIO)
import SubmitHttpApi.Modules.TxProcessor (TxProcessor(..))
import SubmitHttpApi.Config.AppConfig (HttpConfig (..))

type SubmitApi = "submitTx" :> ReqBody '[JSON] TxCreationRequest :> Post '[JSON] TxCreationResponse

userAPI :: Proxy SubmitApi
userAPI = Proxy

submitApiServerT :: TxProcessor IO -> ServerT SubmitApi IO
submitApiServerT TxProcessor{..} = processCreationRequest

submitApiServer :: TxProcessor IO -> Server SubmitApi
submitApiServer processor = hoistServer userAPI liftIO (submitApiServerT processor)

runHttpServer :: TxProcessor IO -> HttpConfig -> IO ()
runHttpServer processor HttpConfig{..} = run (fromIntegral port) (serve userAPI (submitApiServer processor))