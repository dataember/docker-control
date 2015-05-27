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

{-    :<|> "endpoint" :> ReqBody '[JSON] Endpoint :> Put '[JSON] Endpoint
-}

dockerControlApiServer :: PoolConfig -> Server DockerControlAPI
dockerControlApiServer poolCfg = enter (liftNat . (runReaderTNat poolCfg)) dockerControlApiServer'

dockerControlApiServer' :: ServerT DockerControlAPI (ReaderT PoolConfig IO)
dockerControlApiServer' = endpoints :<|> endpointPost

{-    :<|> endpointPost
    :<|> endpointPut
-}

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

{-
endpointPut :: Endpoint -> EitherT ServantErr IO Endpoint
endpointPut _ = return $ Endpoint "docker" 2222
-}

{-
dbConfigToEither' :: forall a. Reader PoolConfig a -> EitherT ServantErr IO a
dbConfigToEither' conf = return $ runReader conf def

dbConfigToEither :: Reader PoolConfig :~> EitherT ServantErr IO
dbConfigToEither = Nat . dbConfigToEither'-}
