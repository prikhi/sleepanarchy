{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Setup where

import           Control.Arrow                  ( (&&&) )
import           Control.Exception.Safe         ( MonadCatch
                                                , MonadThrow
                                                )
import           Control.Monad                  ( forM_
                                                , void
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , runExceptT
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Logger           ( runNoLoggingT )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , runReaderT
                                                )
import           Data.Maybe                     ( isJust
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

import           Models.DB                      ( migrateAll
                                                , tableDefs
                                                )

import qualified Data.ByteString.Char8         as BC
import qualified Data.List                     as L


-- | Monad that tests can run under.
newtype TestM a
    = TestM
        { runTestM :: ReaderT (Pool SqlBackend) (ExceptT ServerError IO) a
        } deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Pool SqlBackend), MonadError ServerError, MonadThrow, MonadCatch)

-- | Evaluate a test action into IO.
--
-- Connect to the test database, drops all it's tables, runs the default
-- migration to recreate the dropped tables, & then evaluates the given
-- TestM action.
testRunner :: TestM a -> IO (Either ServerError a)
testRunner action = do
    dbPassword <- maybe "" ((" password=" <>) . BC.pack) <$> lookupEnv "DB_PASS"
    pool       <- runNoLoggingT $ createPostgresqlPool
        (  "host=localhost user=sleepanarchy-blog dbname=sleepanarchy-blog-test"
        <> dbPassword
        )
        1
    void $ runSqlPool (dropAllTables >> runMigrationQuiet migrateAll) pool
    runExceptT $ runReaderT (runTestM action) pool

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
        mapMaybe (getForeignRef . fieldReference) $ getEntityFields entDef
    -- Extract the Haskell name for a table from a foreign key reference.
    --
    -- Note: ignores SelfReference because that shouldn't affect dropping.
    -- Note: ignores EmbedRef because I dunno what that is.
    getForeignRef :: ReferenceDef -> Maybe EntityNameHS
    getForeignRef = \case
        ForeignRef r -> Just r
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
