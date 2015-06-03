{-# LANGUAGE
    DataKinds
    , DeriveGeneric
    , ExistentialQuantification
    , MultiParamTypeClasses
    , OverloadedStrings
    , TypeFamilies
    , TypeOperators #-}

module API (
    DockerControlAPI
    , DockerControlConfig(..)
    , dockerControlApiServer
    ) where

import Control.Category
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import Data.Default
import Data.List
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite
import qualified Data.Text as T
import GHC.Generics
import qualified Network.HTTP.Conduit as HC
import Network.Wai
import Network.Wai.Handler.Warp
import Network.URL
import Servant

import Prelude hiding ((.))
import Debug.Trace
import DB

import Docker.Conduit.Client
import Docker.Conduit.Info
import Docker.Conduit.Types
import Docker.JSON.Types

type DockerControlAPI = "endpoint" :> Get '[JSON] [Endpoint]
    :<|> "endpoint" :> ReqBody '[JSON] Endpoint :> Post '[JSON] Int
    :<|> "endpoint" :> Capture "endpointId" Int :> Get '[JSON] (Maybe Endpoint)


type DockerControlR r m a = ReaderT r m a


data DockerControlConfig = DockerControlConfig
    { manager  :: HC.Manager
    , connPool :: ConnectionPool
    }
dcManager  = manager
dcConnPool = dcConnPool

dockerControlApiServer :: HC.Manager -> ConnectionPool -> Server DockerControlAPI
dockerControlApiServer manager connPool =
        enter (liftNat . (runReaderTNat $ initConfig connPool manager))
            dockerControlApiServer'
  where
    initConfig :: ConnectionPool -> HC.Manager -> DockerControlConfig
    initConfig connPool manager =
        DockerControlConfig manager connPool


dockerControlApiServer' :: ServerT DockerControlAPI (ReaderT DockerControlConfig IO)
dockerControlApiServer' = endpoints
    :<|> endpointPost
    :<|> endpointGetId


-- | List all endpoints
endpoints :: DockerControlR DockerControlConfig IO [Endpoint]
endpoints = do
    dcCfg <- ask
    lift $ do
        connPool <- dcConnPool dcCfg
        eitherEps <- listAllEndpoints connPool
        case eitherEps of
            Right eps -> return eps
            -- FIXME : Should return 500, or some error resp
            Left _    -> return []


endpointPost :: Endpoint -> ReaderT DockerControlConfig IO Int
endpointPost ep = do
    dcCfg <- ask
    lift $ do
        connPool <- dcConnPool dcCfg
        eitherEp <- insertEndpoint connPool ep
        case eitherEp of
            Right epk -> return . fromIntegral . fromSqlKey $ epk
            -- FIXME : Need to deal with errors
            Left _ -> return 0


endpointGetId :: Int -> ReaderT DockerControlConfig IO (Maybe Endpoint)
endpointGetId key = do
    dcCfg <- ask
    lift $ do
        connPool <- dcConnPool dcCfg
        maybeEp <- getEndpointByKey connPool key
        return maybeEp

daemonInfo :: Int -> ReaderT DockerControlConfig IO (Maybe DockerDaemonInfo)
daemonInfo key = do
    dcCfg <- ask
    lift $ do
        maybeEndpoint <- getEndpointByKey (connPool dcCfg) key
        case maybeEndpoint of
            Just ep -> daemonInfo' (clientCfg ep (manager dcCfg))
            Nothing -> Nothing
  where
    clientCfg ep manager =
        consClientCfg (T.unpack $ endpointHostname ep)
                      (toInteger $ endpointPort ep)
                      manager

daemonInfo' :: DockerClientConfig -> IO (Maybe DockerDaemonInfo)
daemonInfo' cfg = do
    r <- runClient cfg info
    status <- responseStatus r
    case status of
        200 -> liftIO $ daemonInfo'' r
        500 -> return $ (Nothing::Maybe DockerDaemonInfo)

daemonInfo'' ::_
daemonInfo'' r = do
    val <- HC.responseBody r $$+- CA.sinkParser json
    case fromJSON val of
        Success ddi -> return $ Just ddi
        _           -> return $ Nothing

consClientCfg :: String -> Integer -> (HC.Manager -> DockerClientConfig)
consClientCfg host port =
    \manager -> DockerClientConfig manager (Host (HTTP False) host (Just port))
