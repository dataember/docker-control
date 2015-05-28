{-# LANGUAGE
    DataKinds
    , DeriveGeneric
    , ExistentialQuantification
    , MultiParamTypeClasses
    , OverloadedStrings
    , TypeFamilies
    , TypeOperators #-}

module API where

import Control.Category
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Data.Aeson
import Data.Default
import Data.List
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite
import qualified Data.Text as T
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Prelude hiding ((.))
import Debug.Trace
import DB

type DockerControlAPI = "endpoint" :> Get '[JSON] [Endpoint]
    :<|> "endpoint" :> ReqBody '[JSON] Endpoint :> Post '[JSON] Int
    :<|> "endpoint" :> Capture "endpointId" Int :> Get '[JSON] (Maybe Endpoint)

dockerControlApiServer :: PoolConfig -> Server DockerControlAPI
dockerControlApiServer poolCfg = enter (liftNat . (runReaderTNat poolCfg)) dockerControlApiServer'

dockerControlApiServer' :: ServerT DockerControlAPI (ReaderT PoolConfig IO)
dockerControlApiServer' = endpoints
    :<|> endpointPost
    :<|> endpointIdGet


-- | List all endpoints
endpoints :: ReaderT PoolConfig IO [Endpoint]
endpoints = do
    poolCfg <- ask
    lift $ do
        connPool <- pool poolCfg
        eitherEps <- listAllEndpoints connPool
        case eitherEps of
            Right eps -> return eps
            -- FIXME : Should return 500, or some error resp
            Left _    -> return []


endpointPost :: Endpoint -> ReaderT PoolConfig IO Int
endpointPost ep = do
    poolCfg <- ask
    lift $ do
        connPool <- pool poolCfg
        eitherEp <- insertEndpoint connPool ep
        case eitherEp of
            Right epk -> return . fromIntegral . fromSqlKey $ epk
            -- FIXME : Need to deal with errors
            Left _ -> return 0


endpointIdGet :: Int -> ReaderT PoolConfig IO (Maybe Endpoint)
endpointIdGet key = do
    poolCfg <- ask
    lift $ do
        connPool <- pool poolCfg
        maybeEp <- getEndpointByKey connPool key
        return maybeEp
