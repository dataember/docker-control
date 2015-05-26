{-# LANGUAGE
    DataKinds
    , DeriveGeneric
    , OverloadedStrings
    , TypeFamilies
    , TypeOperators #-}

module Main where

import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
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

server :: Server API.ManagementAPI
server = API.managementApiServer

managementAPI :: Proxy API.ManagementAPI
managementAPI = Proxy

app :: Application
app = serve managementAPI server

main :: IO ()
main = do
    connPool <- runStdoutLoggingT $ createSqlitePool ":memory:" 5
    runSql connPool $ runMigration migrateAll
    run 8081 app
