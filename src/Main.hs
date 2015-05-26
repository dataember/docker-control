{-# LANGUAGE
    DataKinds
    , DeriveGeneric
    , OverloadedStrings
    , TypeFamilies
    , TypeOperators #-}

module Main where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.List
import qualified Data.Text as T
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import API

server :: Server ManagementAPI
server = return endpoints

managementAPI :: Proxy ManagementAPI
managementAPI = Proxy

app :: Application
app = serve managementAPI server

main :: IO ()
main = run 8081 app
