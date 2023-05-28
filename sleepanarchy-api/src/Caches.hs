module Caches
    ( Caches (..)
    , initializeCache
    , regenerateBlogSidebarCache
    , BlogSidebarData
    , getBlogSidebarData
    ) where

import GHC.Generics (Generic)

import Control.Monad.IO.Class (MonadIO)
import Database.Persist.Sql (SqlPersistT)

import Caches.Blog (BlogSidebarData, getBlogSidebarData)


newtype Caches = Caches
    { cBlogSidebarData :: BlogSidebarData
    }
    deriving (Show, Read, Eq, Ord, Generic)


initializeCache :: MonadIO m => SqlPersistT m Caches
initializeCache = Caches <$> getBlogSidebarData


regenerateBlogSidebarCache :: MonadIO m => Caches -> SqlPersistT m Caches
regenerateBlogSidebarCache cs =
    (\bsd -> cs {cBlogSidebarData = bsd}) <$> getBlogSidebarData
