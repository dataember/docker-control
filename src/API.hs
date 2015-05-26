{-# LANGUAGE
    DataKinds
    , DeriveGeneric
    , OverloadedStrings
    , TypeFamilies
    , TypeOperators #-}

module API where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.List
import qualified Data.Text as T
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import DB

type ManagementAPI = "endpoint" :> Get '[JSON] [Endpoint]
    :<|> "endpoint" :> ReqBody '[JSON] Endpoint :> Post '[JSON] Endpoint
    :<|> "endpoint" :> ReqBody '[JSON] Endpoint :> Put '[JSON] Endpoint


managementApiServer :: Server ManagementAPI
managementApiServer = endpoints
    :<|> endpointPost
    :<|> endpointPut


endpointPost :: Endpoint -> EitherT ServantErr IO Endpoint
endpointPost _ = return $ Endpoint "docker" 2222

endpointPut :: Endpoint -> EitherT ServantErr IO Endpoint
endpointPut _ = return $ Endpoint "docker" 2222

endpoints :: EitherT ServantErr IO [Endpoint]
endpoints = return
    [ Endpoint "docker" 2456
    ]


