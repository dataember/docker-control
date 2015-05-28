{-# LANGUAGE
    DataKinds
    , DeriveGeneric
    , OverloadedStrings
    , TypeFamilies
    , TypeOperators #-}

module Main where

import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Default
import Data.List
import Database.Persist
import Database.Persist.Postgresql
import qualified Data.Text as T
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import qualified API as API
import DB


server :: PoolConfig -> Server API.DockerControlAPI
server = API.dockerControlApiServer


managementAPI :: Proxy API.DockerControlAPI
managementAPI = Proxy


app :: PoolConfig -> Application
app poolConfig = serve managementAPI (server poolConfig)


data PsqlConnDetails = PsqlConnDetails
    { psqlHost     :: String
    , psqlPort     :: Int
    , psqlDbName   :: String
    , psqlUser     :: String
    , psqlPassword :: String
    } deriving (Eq)

instance Show PsqlConnDetails where
    show (PsqlConnDetails h p d u pwd)  =
        "host=" ++ h ++ " port=" ++ (show p) ++
        " dbname=" ++ d ++ " user=" ++ u ++
        " password=" ++ pwd

instance Default PsqlConnDetails where
    def = PsqlConnDetails
        { psqlHost   = "postgres"
        , psqlPort   = 5432
        , psqlUser   = "postgres"
        , psqlDbName = "postgres"
        , psqlPassword = "test"
        }

main :: IO ()
main =
    let connStr = BS.pack $ show (def::PsqlConnDetails)
    in runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> do
        runSql pool $ runMigration migrateAll
        liftIO $ run 8081 (app $ PoolConfig { pool = return pool })

