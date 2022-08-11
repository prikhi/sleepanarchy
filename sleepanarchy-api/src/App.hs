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
import           Servant.Server                 ( Handler )

import           Models.DB                      ( migrateAll )


newtype Config = Config
    { cfgDbPool :: Pool SqlBackend
    }

mkConfig :: IO Config
mkConfig = do
    cfgDbPool <- runStdoutLoggingT $ createPostgresqlPool
        "host=localhost user=sleepanarchy-blog dbname=sleepanarchy-blog"
        20
    runSqlPool (runMigration migrateAll) cfgDbPool
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
