{-# LANGUAGE EmptyDataDecls
    , FlexibleContexts
    , FlexibleInstances
    , GADTs
    , GeneralizedNewtypeDeriving
    , MultiParamTypeClasses
    , OverloadedStrings
    , QuasiQuotes
    , TemplateHaskell
    , TypeFamilies #-}

module DB where

import Control.Exception
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Default
import Database.Persist
--import Database.Persist.Sqlite
import Database.Persist.Postgresql
import Database.Persist.TH
import qualified Data.Text as T


type DBConfig a = Reader PoolConfig a

data PoolConfig = PoolConfig
    { pool :: IO ConnectionPool
    }


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Endpoint json
    hostname T.Text
    port Int
    UniqueEndpoint hostname port
    deriving Show
|]


runSql :: MonadBaseControl IO m => ConnectionPool -> SqlPersistT m a -> m a
runSql pool sql = runSqlPool sql pool


insertEndpoint :: ConnectionPool -> Endpoint -> IO (Either String (Key Endpoint))
insertEndpoint pool ep = do
    bool <- doesEndpointExist pool ep
    case bool of
        True -> return $ Left ("Endpoint " ++ show ep ++ " already exists!")
        False-> do
            e <- tryInsert ep
            case e of
                Right a -> return $ Right a
                Left ex -> return $ Left ("An exception of " ++ show ex ++ " occured.")
  where
    tryInsert :: Endpoint -> IO (Either PersistentSqlException (Key Endpoint))
    tryInsert ep = try (runSql pool $ insert $ ep)


doesEndpointExist :: ConnectionPool -> Endpoint -> IO Bool
doesEndpointExist pool ep =  do
        maybeEndpoint <- runSql pool $ getBy $ UniqueEndpoint (endpointHostname ep) (endpointPort ep)
        case maybeEndpoint of
            Just _  -> return True
            Nothing -> return False


getEndpointByKey :: ConnectionPool -> Int -> IO (Maybe Endpoint)
getEndpointByKey connPool i = runSql connPool $ get $ toSqlKey (fromIntegral i)


listAllEndpoints :: ConnectionPool -> IO (Either String [Endpoint])
listAllEndpoints pool = do
    endpoints <- trySelect
    case endpoints of
        Right eps -> return $ Right $ fmap (entityVal) eps
        Left ex   -> return $ Left ("An exception of " ++ show ex ++ " occured.")
  where
    trySelect :: IO (Either PersistentSqlException [Entity Endpoint])
    trySelect = try $ runSql pool $ selectList ([] :: [Filter Endpoint]) []


