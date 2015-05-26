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
import Control.Monad.Trans.Control
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.Text as T

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
            e <- try (runSql pool $ insert $ ep) :: IO (Either PersistentSqlException (Key Endpoint))
            case e of
                Right a -> return $ Right a
                Left ex -> return $ Left ("An exception of " ++ show ex ++ " occured.")


doesEndpointExist :: ConnectionPool -> Endpoint -> IO Bool
doesEndpointExist pool ep =  do
        maybeEndpoint <- runSql pool $ getBy $ UniqueEndpoint (endpointHostname ep) (endpointPort ep)
        case maybeEndpoint of
            Just _  -> return True
            Nothing -> return False
