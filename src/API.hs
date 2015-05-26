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

type ManagementAPI =
    "endpoint" :> Get '[JSON] [Endpoint]


endpoints :: [Endpoint]
endpoints =
    [ Endpoint "docker" 2456
    ]


