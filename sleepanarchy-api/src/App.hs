{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module App
    ( Config (..)
    , mkConfig
    , Environment (..)
    , App (..)

      -- * DB
    , HasDbPool (..)
    , DB (..)
    , DBThrows (..)

      -- * Errors
    , ThrowsError (..)

      -- * Auth
    , HasJwk (..)
    , HasCookieSettings (..)
    , AuthToken (..)

      -- * Media
    , HasMediaDir (..)
    , Media (..)
    , MediaSubPath
    , fromMediaSubPath
    , foldersToSubPath

      -- * Cache
    , HasCachesTVar (..)
    , HasCachesTVarM (..)
    , Cache (..)
    ) where

import Control.Concurrent.STM
    ( TVar
    , atomically
    , modifyTVar
    , newTVarIO
    , readTVarIO
    )
import Control.Exception.Safe (MonadCatch, MonadThrow, try)
import Control.Lens ((^.))
import Control.Monad (unless, (>=>))
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Logger (NoLoggingT (runNoLoggingT), runStdoutLoggingT)
import Control.Monad.Reader (MonadIO (..), MonadReader, ReaderT, asks)
import Crypto.JOSE.JWK (JWK)
import Data.Aeson (decode)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlPool, showMigration)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant (Application, ServerError, throwError)
import Servant.Auth.Server
    ( CookieSettings (..)
    , JWTSettings
    , defaultCookieSettings
    , defaultJWTSettings
    , generateKey
    )
import Servant.Server (Handler)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , doesPathExist
    , listDirectory
    , makeAbsolute
    )
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath
    ( addTrailingPathSeparator
    , joinPath
    , normalise
    , splitPath
    )
import System.FilePath.Lens (directory)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import Caches
    ( BlogSidebarData
    , Caches (..)
    , getBlogSidebarData
    , initializeCache
    )
import Models.DB (migrateAll)

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy.Char8 qualified as LBC
import Data.Text qualified as T


data Environment
    = Production
    | Development
    deriving (Show, Read, Eq)


data Config = Config
    { cfgEnv :: Environment
    , cfgDbPool :: Pool SqlBackend
    , cfgJwk :: JWK
    , cfgCookieSettings :: CookieSettings
    , cfgMediaDirectory :: FilePath
    , cfgCaches :: TVar Caches
    , cfgLoggingMiddleware :: Application -> Application
    }


mkConfig :: IO Config
mkConfig = do
    env <- fromMaybe Development . (>>= readMaybe) <$> lookupEnv "ENVIRONMENT"
    let cfgLoggingMiddleware = case env of
            Development -> logStdoutDev
            Production -> logStdout
    connStr <- dbConnectionString
    cfgDbPool <- case env of
        Development ->
            runStdoutLoggingT $ createPostgresqlPool connStr 2
        Production ->
            runNoLoggingT $ createPostgresqlPool connStr 20
    cfgCaches <- flip runSqlPool cfgDbPool $ do
        migrationsDue <- showMigration migrateAll
        unless (null migrationsDue) $ liftIO $ do
            putStrLn "Cannot start server due to unmigrated database:"
            mapM_ (putStrLn . ("\t" <>) . T.unpack) migrationsDue
            putStrLn "Create a migration with `sql-migrate new`."
            putStrLn "Run a migration with `sql-migrate up`, then try again."
            exitFailure
        initializeCache >>= liftIO . newTVarIO
    cfgJwk <-
        lookupEnv "API_JWK" >>= \case
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
    let cfgCookieSettings =
            defaultCookieSettings
                { cookieXsrfSetting = Nothing
                , cookieMaxAge = Just $ 60 * 60 * 24 * 365
                }
    cfgMediaDirectory <- determineMediaDirectory
    return Config {cfgEnv = env, ..}
  where
    -- Determine the database hostname from the @DB_HOST@ env var, falling
    -- back to @localhost@. Then assemble & return the connection string.
    --
    -- TODO: abstract this flow of lookup->print stderr->default?
    dbConnectionString :: IO ConnectionString
    dbConnectionString = do
        host <-
            lookupEnv "DB_HOST" >>= \case
                Nothing ->
                    hPutStrLn stderr "`DB_HOST` environmental variable not set - using localhost"
                        >> return "localhost"
                Just x -> return $ BC.pack x
        return $ "host=" <> host <> " user=sleepanarchy-blog dbname=sleepanarchy-blog"
    -- Determine & create the media directory if necessary. Checks the
    -- @MEDIA_DIRECTORY@ environment variable, defaulting to @./media@ if
    -- not defined. Exits with an error if the media directory is actually
    -- a file.
    determineMediaDirectory :: IO FilePath
    determineMediaDirectory = do
        rawDir <-
            lookupEnv "MEDIA_DIRECTORY" >>= \case
                Nothing ->
                    hPutStrLn
                        stderr
                        "`MEDIA_DIRECTORY` environmental variable not set - using ./media"
                        >> return "./media"
                Just x -> return x
        dir <- addTrailingPathSeparator . normalise <$> makeAbsolute rawDir
        (exists, isFolder) <-
            (,) <$> doesPathExist dir <*> doesDirectoryExist dir
        if
            | isFolder ->
                return dir
            | exists ->
                putStrLn ("Specified media directory is a file: " <> dir)
                    >> exitFailure
            | otherwise ->
                putStrLn ("Specified media directory will be created: " <> dir)
                    >> createDirectoryIfMissing True dir
                    >> return dir


newtype App a = App
    { runApp :: ReaderT Config Handler a
    }
    deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadError ServerError, MonadThrow, MonadCatch)


class HasDbPool a where
    getDbPool :: a -> Pool SqlBackend


instance HasDbPool Config where
    getDbPool = cfgDbPool


instance HasDbPool (Pool SqlBackend) where
    getDbPool = id


class DB m where
    -- | Run a series of queries in a transaction. May throw an SqlError.
    runDB :: SqlPersistT IO a -> m a


class DBThrows m where
    -- | Run a series of queries in a transaction. Thrown 'ServerError's
    -- are caught & re-thrown in @m@.
    runDBThrow :: SqlPersistT IO a -> m a


instance (HasDbPool cfg, MonadReader cfg m, MonadIO m) => DB m where
    runDB query = asks getDbPool >>= liftIO . runSqlPool query


instance (DB m, MonadCatch m, ThrowsError m) => DBThrows m where
    runDBThrow =
        try . runDB >=> \case
            Right r -> return r
            Left e -> serverError e


class ThrowsError m where
    serverError :: ServerError -> m a


instance ThrowsError App where
    serverError = throwError


instance Monad m => ThrowsError (ExceptT ServerError m) where
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
    getJWTSettings = asks (defaultJWTSettings . getJwk)
    getCookieSettings = asks getCookieSettings_


-- | Knows of the media directory.
class HasMediaDir a where
    -- | Get the base media directory.
    getMediaDir :: a -> FilePath


instance HasMediaDir Config where
    getMediaDir = cfgMediaDirectory


-- | Filesystem related actions contained within the media directory.
class Media m where
    -- Check a file or directory path exists.
    subPathExists :: MediaSubPath -> m Bool


    -- Check a file exists.
    subFileExists :: MediaSubPath -> m Bool


    -- Check a directory exists.
    subDirectoryExists :: MediaSubPath -> m Bool


    -- List the files & directories in a path.
    subDirectoryContents :: MediaSubPath -> m [FilePath]


    -- Write the file contents to the given subpath.
    writeFileToSubPath :: MediaSubPath -> BS.ByteString -> m ()


    -- Create the given sub-directories.
    createSubDirectory :: MediaSubPath -> m ()


-- | Warning: some of these will throw IOErrors but those are not captured
-- in the type.
instance (HasMediaDir cfg, MonadReader cfg m, MonadIO m) => Media m where
    subPathExists subPath = withMediaDir subPath >>= liftIO . doesPathExist
    subDirectoryExists subPath =
        withMediaDir subPath >>= liftIO . doesDirectoryExist
    subFileExists subPath = withMediaDir subPath >>= liftIO . doesFileExist
    subDirectoryContents subPath =
        withMediaDir subPath >>= liftIO . listDirectory
    writeFileToSubPath subPath contents = do
        path <- withMediaDir subPath
        let dir = path ^. directory
        liftIO $ createDirectoryIfMissing True dir >> BC.writeFile path contents
    createSubDirectory subPath =
        withMediaDir subPath >>= liftIO . createDirectoryIfMissing True


withMediaDir
    :: (HasMediaDir cfg, MonadReader cfg m) => MediaSubPath -> m FilePath
withMediaDir (MediaSubPath subDir) = do
    asks ((<> subDir) . getMediaDir)


-- | Some directory or file path within the media directory.
newtype MediaSubPath = MediaSubPath
    { _fromMediaSubPath :: FilePath
    }
    deriving (Show, Read, Eq, Ord)


-- | Pull the relative FilePath from a MediaSubPath.
fromMediaSubPath :: MediaSubPath -> FilePath
fromMediaSubPath = _fromMediaSubPath


-- | Convert a list of folder names & optional ending file into a subpath
-- in the media directory.
--
-- Filters out an parent accessors & normalizes the path.
foldersToSubPath :: [FilePath] -> MediaSubPath
foldersToSubPath =
    MediaSubPath
        . normalise
        . joinPath
        . concatMap (filter (`notElem` ["..", "../"]) . splitPath)


-- | Knows of the global 'Caches' 'TVar'.
class HasCachesTVar a where
    -- | Get the thread-safe cache.
    getCaches :: a -> TVar Caches


instance HasCachesTVar Config where
    getCaches = cfgCaches


class HasCachesTVarM m where
    getCachesM :: m (TVar Caches)


instance (HasCachesTVar a, MonadReader a m) => HasCachesTVarM m where
    getCachesM = asks getCaches


class Cache m where
    getBlogSidebarCache :: m BlogSidebarData
    bustBlogSidebarCache :: m ()


instance (HasCachesTVarM m, MonadIO m, DB m) => Cache m where
    getBlogSidebarCache = do
        tCache <- getCachesM
        fmap cBlogSidebarData . liftIO $ readTVarIO tCache
    bustBlogSidebarCache = do
        tCache <- getCachesM
        bsd <- runDB getBlogSidebarData
        liftIO . atomically . modifyTVar tCache $ \c ->
            c {cBlogSidebarData = bsd}
