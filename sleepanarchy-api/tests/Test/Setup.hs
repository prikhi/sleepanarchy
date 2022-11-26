{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Setup where

import           Control.Arrow                  ( (&&&) )
import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , modifyTVar
                                                , newTVarIO
                                                , readTVarIO
                                                )
import           Control.Exception.Safe         ( MonadCatch
                                                , MonadThrow
                                                )
import           Control.Monad                  ( forM_
                                                , join
                                                , void
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , runExceptT
                                                , throwError
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Logger           ( runNoLoggingT )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , ask
                                                , lift
                                                , runReaderT
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , mapMaybe
                                                )
import           Data.Pool                      ( Pool )
import           Database.Persist.Postgresql    ( createPostgresqlPool )
import           Database.Persist.Sql           ( EntityDef
                                                , EntityNameDB(..)
                                                , EntityNameHS
                                                , ReferenceDef(..)
                                                , SqlBackend
                                                , SqlPersistT
                                                , fieldReference
                                                , getEntityDBName
                                                , getEntityFields
                                                , getEntityHaskellName
                                                , rawExecute
                                                , runMigrationQuiet
                                                , runSqlPool
                                                )
import           Servant                        ( ServerError )
import           System.Environment             ( lookupEnv )
import           System.FilePath                ( dropTrailingPathSeparator
                                                , splitPath
                                                )

import           App                            ( Media(..)
                                                , ThrowsError(..)
                                                , foldersToSubPath
                                                , fromMediaSubPath
                                                )
import           Models.DB                      ( migrateAll
                                                , tableDefs
                                                )

import qualified Data.ByteString.Char8         as BC
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as L


-- | Monad that tests can run under.
newtype TestM a
    = TestM
        { runTestM :: ReaderT (Pool SqlBackend) (ReaderT (TVar InMemoryDirectory) (ExceptT ServerError IO)) a
        } deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Pool SqlBackend), MonadError ServerError, MonadThrow, MonadCatch)

instance ThrowsError TestM where
    serverError = throwError

-- | Evaluate a test action into IO.
--
-- Connect to the test database, drops all it's tables, runs the default
-- migration to recreate the dropped tables, & then evaluates the given
-- TestM action.
testRunner :: TestM a -> IO (Either ServerError a)
testRunner = fmap snd . customTestRunner (InMemoryDirectory HM.empty)

-- | 'testRunner', but allows you to pass in some custom data such as
-- a non-empty Media filesystem & returns the final filesystem.
customTestRunner
    :: InMemoryDirectory
    -> TestM a
    -> IO (InMemoryDirectory, Either ServerError a)
customTestRunner mediaFS action = do
    dbPassword <- maybe "" ((" password=" <>) . BC.pack) <$> lookupEnv "DB_PASS"
    pool       <- runNoLoggingT $ createPostgresqlPool
        (  "host=localhost user=sleepanarchy-blog dbname=sleepanarchy-blog-test"
        <> dbPassword
        )
        1
    mediaFileSystem <- newTVarIO mediaFS
    void $ runSqlPool (dropAllTables >> runMigrationQuiet migrateAll) pool
    result <- runExceptT
        $ runReaderT (runReaderT (runTestM action) pool) mediaFileSystem
    finalMediaFileSystem <- readTVarIO mediaFileSystem
    return (finalMediaFileSystem, result)

newtype InMemoryDirectory
    = InMemoryDirectory (HM.HashMap FileName FSNode)
    deriving (Show, Read, Eq, Ord)

type FileName = String
data FSNode
    = FileNode String
    -- ^ File with contents
    | DirectoryNode InMemoryDirectory
    -- ^ Directory with contents
    deriving (Show, Read, Eq, Ord)

instance {-# OVERLAPPING #-} Media TestM where
    subPathExists subPath
        | subPath == foldersToSubPath [] = return True
        | otherwise = do
            tv <- TestM $ lift ask
            m  <- liftIO $ readTVarIO tv
            let pathParts =
                    map dropTrailingPathSeparator . splitPath $ fromMediaSubPath
                        subPath
            return $ fromMaybe False $ recursePath (const True) m pathParts
    subFileExists subPath
        | subPath == foldersToSubPath [] = return False
        | otherwise = do
            tv <- TestM $ lift ask
            m  <- liftIO $ readTVarIO tv
            let pathParts =
                    map dropTrailingPathSeparator . splitPath $ fromMediaSubPath
                        subPath
            return $ fromMaybe False $ recursePath
                (\case
                    FileNode      _ -> True
                    DirectoryNode _ -> False
                )
                m
                pathParts
    subDirectoryExists subPath
        | subPath == foldersToSubPath [] = return False
        | otherwise = do
            tv <- TestM $ lift ask
            m  <- liftIO $ readTVarIO tv
            let pathParts =
                    map dropTrailingPathSeparator . splitPath $ fromMediaSubPath
                        subPath
            return $ fromMaybe False $ recursePath
                (\case
                    FileNode      _ -> False
                    DirectoryNode _ -> True
                )
                m
                pathParts
    subDirectoryContents subPath = do
        tv <- TestM $ lift ask
        m  <- liftIO $ readTVarIO tv
        let pathParts =
                map dropTrailingPathSeparator . splitPath $ fromMediaSubPath
                    subPath
        return $ fromMaybe [] $ join $ recursePath
            (\case
                FileNode _ -> Nothing
                DirectoryNode (InMemoryDirectory subDirMap) ->
                    Just $ HM.keys subDirMap
            )
            m
            pathParts
    writeFileToSubPath subPath contents = do
        let pathParts =
                map dropTrailingPathSeparator . splitPath $ fromMediaSubPath
                    subPath
        tv <- TestM $ lift ask
        liftIO . atomically $ modifyTVar tv $ \m -> recurseAndAlterPath
            (\case
                Just n@DirectoryNode{} -> Just n
                _                      -> Just $ FileNode $ BC.unpack contents
            )
            m
            pathParts
    createSubDirectory subPath = do
        let pathParts =
                map dropTrailingPathSeparator . splitPath $ fromMediaSubPath
                    subPath
        tv <- TestM $ lift ask
        liftIO . atomically $ modifyTVar tv $ \m -> recurseAndAlterPath
            (\case
                Nothing -> Just $ DirectoryNode $ InMemoryDirectory HM.empty
                x       -> x
            )
            m
            pathParts

recursePath :: (FSNode -> a) -> InMemoryDirectory -> [FilePath] -> Maybe a
recursePath onFind (InMemoryDirectory dirMap) = \case
    []                   -> Nothing
    [       fileName]    -> onFind <$> HM.lookup fileName dirMap
    nextDir :       rest -> case HM.lookup nextDir dirMap of
        Nothing                        -> Nothing
        Just (FileNode      _        ) -> Nothing
        Just (DirectoryNode subDirMap) -> recursePath onFind subDirMap rest

recurseAndAlterPath
    :: (Maybe FSNode -> Maybe FSNode)
    -> InMemoryDirectory
    -> [FilePath]
    -> InMemoryDirectory
recurseAndAlterPath onFind imd@(InMemoryDirectory dirMap) = \case
    []                   -> imd
    [       fileName]    -> InMemoryDirectory $ HM.alter onFind fileName dirMap
    nextDir :       rest -> InMemoryDirectory $ HM.alter
        (\case
            Nothing -> Just . DirectoryNode $ recurseAndAlterPath
                onFind
                (InMemoryDirectory HM.empty)
                rest
            Just (FileNode fn) -> Just (FileNode fn)
            Just (DirectoryNode subDirMap) ->
                Just . DirectoryNode $ recurseAndAlterPath onFind subDirMap rest
        )
        nextDir
        dirMap


-- | Helper function to clear out all Persistent tables from a database.
dropAllTables :: MonadIO m => SqlPersistT m ()
dropAllTables = forM_ tableDropOrder $ \tableName -> rawExecute
    ("DROP TABLE IF EXISTS \"" <> unEntityNameDB tableName <> "\";")
    []
  where
    -- Ordered list of table names to drop. Front of list has tables w/
    -- fields that point to other tables & thus need to be dropped first.
    tableDropOrder :: [EntityNameDB]
    tableDropOrder =
        let entitiesWithRefs :: [(EntityDef, [EntityNameHS])]
            entitiesWithRefs = map (id &&& getForeignRefs) tableDefs
            noRefs :: [(EntityDef, [EntityNameHS])]
            hasRefs :: [(EntityDef, [EntityNameHS])]
            (noRefs, hasRefs) = L.partition (null . snd) entitiesWithRefs
        in  map (getEntityDBName . fst) $ findDropOrder noRefs hasRefs
    -- For a table, get all it's references to other tables.
    getForeignRefs :: EntityDef -> [EntityNameHS]
    getForeignRefs entDef =
        mapMaybe (getForeignRef (getEntityHaskellName entDef) . fieldReference)
            $ getEntityFields entDef
    -- Extract the Haskell name for a table from a foreign key reference.
    --
    -- Note: ignores SelfReference because that shouldn't affect dropping.
    -- Note: ignores EmbedRef because I dunno what that is.
    getForeignRef :: EntityNameHS -> ReferenceDef -> Maybe EntityNameHS
    getForeignRef myName = \case
        ForeignRef r -> if myName == r then Nothing else Just r
        _            -> Nothing
    -- Given a list of tables that can be dropped, iterate over a list of
    -- tables to process, adding any new tables to the drop list
    -- & recursing with the new drop & process lists the to process list is
    -- empty.
    findDropOrder
        :: [(EntityDef, [EntityNameHS])]
        -> [(EntityDef, [EntityNameHS])]
        -> [(EntityDef, [EntityNameHS])]
    findDropOrder canDrop toProcess
        | null toProcess = canDrop
        | otherwise = uncurry findDropOrder
        $ foldr findDropOrderReducer (canDrop, []) toProcess
    -- Determine if we can drop a table. A table is droppable iff all the
    -- table it references are already in the list of tables we can drop.
    findDropOrderReducer
        :: (EntityDef, [EntityNameHS])
        -> ([(EntityDef, [EntityNameHS])], [(EntityDef, [EntityNameHS])])
        -> ([(EntityDef, [EntityNameHS])], [(EntityDef, [EntityNameHS])])
    findDropOrderReducer t@(_, tableRefs) (canDrop, cantDrop) =
        let
            canDropAllRefs = all
                (\ref -> isJust
                    $ L.find ((== ref) . getEntityHaskellName . fst) canDrop
                )
                tableRefs
        in  if canDropAllRefs
                then (t : canDrop, cantDrop)
                else (canDrop, t : cantDrop)
