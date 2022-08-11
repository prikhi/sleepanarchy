{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module App where

import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Control.Monad.Reader           ( MonadIO(..)
                                                , MonadReader
                                                , ReaderT
                                                , asks
                                                )
import           Crypto.JOSE.JWK                ( JWK )
import           Data.Aeson                     ( decode )
import           Data.Pool                      ( Pool )
import           Database.Persist.Postgresql    ( createPostgresqlPool )
import           Database.Persist.Sql           ( SqlBackend
                                                , SqlPersistT
                                                , runMigration
                                                , runSqlPool
                                                )
import           Servant                        ( ServerError
                                                , throwError
                                                )
import           Servant.Auth.Server            ( JWTSettings
                                                , defaultJWTSettings
                                                , generateKey
                                                )
import           Servant.Server                 ( Handler )
import           System.Environment             ( lookupEnv )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

import           Models.DB                      ( migrateAll )

import qualified Data.ByteString.Lazy.Char8    as LBC


data Config = Config
    { cfgDbPool :: Pool SqlBackend
    , cfgJwk    :: JWK
    }

mkConfig :: IO Config
mkConfig = do
    cfgDbPool <- runStdoutLoggingT $ createPostgresqlPool
        "host=localhost user=sleepanarchy-blog dbname=sleepanarchy-blog"
        20
    runSqlPool (runMigration migrateAll) cfgDbPool
    cfgJwk <- lookupEnv "API_JWK" >>= \case
        Nothing -> do
            hPutStrLn
                stderr
                "`API_JWK` environmental variable not set - using ephemeral keys."
            generateKey
        Just rawJwk -> case decode $ LBC.pack rawJwk of
            Nothing -> do
                hPutStrLn
                    stderr
                    "Could not parse `API_JWK` environmental variable - using ephemeral keys."
                generateKey
            Just jwk -> return jwk
    return Config { .. }



newtype App a =
    App
        { runApp :: ReaderT Config Handler a
        } deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadError ServerError)



class HasDbPool a where
    getDbPool :: a -> Pool SqlBackend

instance HasDbPool Config where
    getDbPool = cfgDbPool


class DB m where
    runDB :: SqlPersistT IO a -> m a

instance (HasDbPool cfg, MonadReader cfg m, MonadIO m) => DB m where
    runDB query = asks getDbPool >>= liftIO . runSqlPool query


class ThrowsError m where
    serverError :: ServerError -> m a

instance ThrowsError App where
    serverError = throwError


class HasJwk a where
    getJwk :: a -> JWK

instance HasJwk Config where
    getJwk = cfgJwk


class JWTToken m where
    getJWTSettings :: m JWTSettings

instance (HasJwk cfg, MonadReader cfg m) => JWTToken m where
    getJWTSettings = asks (defaultJWTSettings . getJwk)
