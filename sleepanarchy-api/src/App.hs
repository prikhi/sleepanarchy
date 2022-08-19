{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module App where

import           Control.Monad                  ( unless )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Logger           ( NoLoggingT(runNoLoggingT)
                                                , runStdoutLoggingT
                                                )
import           Control.Monad.Reader           ( MonadIO(..)
                                                , MonadReader
                                                , ReaderT
                                                , asks
                                                )
import           Crypto.JOSE.JWK                ( JWK )
import           Data.Aeson                     ( decode )
import           Data.Maybe                     ( fromMaybe )
import           Data.Pool                      ( Pool )
import           Database.Persist.Postgresql    ( ConnectionString
                                                , createPostgresqlPool
                                                )
import           Database.Persist.Sql           ( SqlBackend
                                                , SqlPersistT
                                                , runSqlPool
                                                , showMigration
                                                )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )
import           Servant                        ( Application
                                                , ServerError
                                                , throwError
                                                )
import           Servant.Auth.Server            ( CookieSettings(..)
                                                , JWTSettings
                                                , defaultCookieSettings
                                                , defaultJWTSettings
                                                , generateKey
                                                )
import           Servant.Server                 ( Handler )
import           System.Environment             ( lookupEnv )
import           System.Exit                    ( exitFailure )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Text.Read                      ( readMaybe )

import           Models.DB                      ( migrateAll )

import qualified Data.ByteString.Lazy.Char8    as LBC
import qualified Data.Text                     as T


data Environment
    = Production
    | Development
    deriving (Show, Read, Eq)

data Config = Config
    { cfgDbPool            :: Pool SqlBackend
    , cfgJwk               :: JWK
    , cfgCookieSettings    :: CookieSettings
    , cfgLoggingMiddleware :: Application -> Application
    }

mkConfig :: IO Config
mkConfig = do
    env <- fromMaybe Development . (>>= readMaybe) <$> lookupEnv "ENVIRONMENT"
    let cfgLoggingMiddleware = case env of
            Development -> logStdoutDev
            Production  -> id
    cfgDbPool <- case env of
        Development ->
            runStdoutLoggingT $ createPostgresqlPool dbConnectionString 2
        Production ->
            runNoLoggingT $ createPostgresqlPool dbConnectionString 20
    flip runSqlPool cfgDbPool $ do
        migrationsDue <- showMigration migrateAll
        unless (null migrationsDue) $ liftIO $ do
            putStrLn "Cannot start server due to unmigrated database:"
            mapM_ (putStrLn . ("\t" <>) . T.unpack) migrationsDue
            putStrLn "Create a migration with `sql-migrate new`."
            putStrLn "Run a migration with `sql-migrate up`, then try again."
            exitFailure
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
    let cfgCookieSettings = defaultCookieSettings
            { cookieXsrfSetting = Nothing
            , cookieMaxAge      = Just $ 60 * 60 * 24 * 365
            }
    return Config { .. }
  where
    dbConnectionString :: ConnectionString
    dbConnectionString =
        "host=localhost user=sleepanarchy-blog dbname=sleepanarchy-blog"



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

class HasCookieSettings a where
    getCookieSettings_ :: a -> CookieSettings

instance HasCookieSettings Config where
    getCookieSettings_ = cfgCookieSettings


class AuthToken m where
    getJWTSettings :: m JWTSettings
    getCookieSettings :: m CookieSettings

instance (HasJwk cfg, HasCookieSettings cfg, MonadReader cfg m) => AuthToken m where
    getJWTSettings    = asks (defaultJWTSettings . getJwk)
    getCookieSettings = asks getCookieSettings_
