{-# LANGUAGE RecordWildCards #-}

module Caches.Blog where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON (..))
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql (Single (..), SqlPersistT, rawSql)
import GHC.Generics (Generic)
import Servant.Docs (ToSample (..), singleSample)
import Text.RawString.QQ (r)

import Models.DB
import Utils (prefixToJSON)

import Database.Esqueleto.Experimental qualified as E


data BlogSidebarData = BlogSidebarData
    { bsdRecent :: [BlogRecentPostData]
    , bsdArchive :: [BlogArchiveYearData]
    , bsdTags :: [BlogTagData]
    , bsdCategories :: [BlogSidebarCategoryData]
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON BlogSidebarData where
    toJSON = prefixToJSON "bsd"


instance ToSample BlogSidebarData where
    toSamples _ =
        singleSample $
            BlogSidebarData
                { bsdRecent =
                    [ BlogRecentPostData "Newest Post" "newest-post"
                    , BlogRecentPostData "Second in line" "second-in-line"
                    , BlogRecentPostData "Oldest in limit query" "oldest-in-limit-query"
                    ]
                , bsdArchive =
                    [ BlogArchiveYearData 2022 8 15
                    , BlogArchiveYearData 2022 1 42
                    , BlogArchiveYearData 2020 6 9001
                    , BlogArchiveYearData 2012 5 1
                    ]
                , bsdTags =
                    [ BlogTagData "Augment" 10
                    , BlogTagData "Haskell" 42
                    , BlogTagData "Purescript" 9001
                    , BlogTagData "Wordpress" 1
                    ]
                , bsdCategories =
                    [ BlogSidebarCategoryData "Guides" "guides" 9
                    , BlogSidebarCategoryData "Home Lab" "home-lab" 2
                    , BlogSidebarCategoryData "Rave Saber" "rave-saber" 121
                    ]
                }


data BlogRecentPostData = BlogRecentPostData
    { brpdTitle :: Text
    , brpdSlug :: Text
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON BlogRecentPostData where
    toJSON = prefixToJSON "brpd"


data BlogArchiveYearData = BlogArchiveYearData
    { baydYear :: Int
    , baydMonth :: Int
    , baydCount :: Int
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON BlogArchiveYearData where
    toJSON = prefixToJSON "bayd"


data BlogTagData = BlogTagData
    { btdTag :: Text
    , btdCount :: Int
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON BlogTagData where
    toJSON = prefixToJSON "btd"


data BlogSidebarCategoryData = BlogSidebarCategoryData
    { bscdTitle :: Text
    , bscdSlug :: Text
    , bscdCount :: Int
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON BlogSidebarCategoryData where
    toJSON = prefixToJSON "bscd"


getBlogSidebarData :: MonadIO m => SqlPersistT m BlogSidebarData
getBlogSidebarData = do
    let mkRecent (Entity _ p) =
            BlogRecentPostData (blogPostTitle p) (blogPostSlug p)
    bsdRecent <-
        map mkRecent
            <$> selectList
                [BlogPostPublishedAt !=. Nothing]
                [Desc BlogPostPublishedAt, LimitTo 5]
    bsdArchive <- getBlogSidebarArchive
    bsdTags <- getBlogSidebarTags
    bsdCategories <- getBlogSidebarCategories
    return BlogSidebarData {..}


getBlogSidebarArchive :: MonadIO m => SqlPersistT m [BlogArchiveYearData]
getBlogSidebarArchive = do
    let archiveQuery =
            [r|
            SELECT
                DATE_PART('year', published_at) AS year,
                DATE_PART('month', published_at) AS month,
                COUNT(*)
            FROM blog_post
            WHERE published_at IS NOT NULL
            GROUP BY year, month
            ORDER BY year DESC, month DESC
            |]
    map mkArchive <$> rawSql archiveQuery []
  where
    mkArchive :: (Single Int, Single Int, Single Int) -> BlogArchiveYearData
    mkArchive (Single y, Single m, Single c) = BlogArchiveYearData y m c


getBlogSidebarTags :: MonadIO m => SqlPersistT m [BlogTagData]
getBlogSidebarTags = do
    let tagQuery =
            [r|
            SELECT *
            FROM (
                SELECT
                    TRIM(FROM UNNEST(STRING_TO_ARRAY(tags, ','))) AS tag,
                    COUNT(*)
                FROM blog_post
                WHERE published_at IS NOT NULL
                GROUP BY tag
                ORDER BY tag ASC
            ) AS sq
            WHERE tag <> ''
            |]
    map mkTag <$> rawSql tagQuery []
  where
    mkTag :: (Single Text, Single Int) -> BlogTagData
    mkTag (Single t, Single c) = BlogTagData t c


getBlogSidebarCategories
    :: MonadIO m => SqlPersistT m [BlogSidebarCategoryData]
getBlogSidebarCategories = do
    let
        categoryQuery = E.select $ do
            (post E.:& category) <-
                E.from
                    $ E.table @BlogPost
                        `E.InnerJoin` E.table @BlogCategory
                    `E.on` ( \(post E.:& category) ->
                                (post E.^. BlogPostCategoryId)
                                    E.==. (category E.^. BlogCategoryId)
                           )
            E.groupBy
                ( category E.^. BlogCategoryTitle
                , category E.^. BlogCategorySlug
                )
            E.orderBy [E.asc $ category E.^. BlogCategoryTitle]
            E.having $ E.countRows @Int E.>. E.val 0
            E.where_ $ E.not_ (E.isNothing $ post E.^. BlogPostPublishedAt)
            return
                ( category E.^. BlogCategoryTitle
                , category E.^. BlogCategorySlug
                , E.countRows
                )
    map mkCategory <$> categoryQuery
  where
    mkCategory
        :: (E.Value Text, E.Value Text, E.Value Int) -> BlogSidebarCategoryData
    mkCategory (E.Value t, E.Value s, E.Value c) =
        BlogSidebarCategoryData t s c
