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
import Data.List
import Database.Persist
import Database.Persist.Sqlite
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


main :: IO ()
main = do
    connPool <- runStdoutLoggingT $ createSqlitePool ":memory:" 5
    let poolConfig = PoolConfig { pool = return connPool }
    runSql connPool $ runMigration migrateAll
    run 8081 (app poolConfig)
