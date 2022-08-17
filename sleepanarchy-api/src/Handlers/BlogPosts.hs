{-# LANGUAGE RecordWildCards #-}
module Handlers.BlogPosts where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Aeson                     ( ToJSON(..) )
import           Data.Maybe                     ( fromJust )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime(..)
                                                , addDays
                                                , fromGregorian
                                                , gregorianMonthLength
                                                )
import           Database.Persist
import           Database.Persist.Sql           ( Single(..)
                                                , SqlPersistT
                                                , rawSql
                                                )
import           GHC.Generics                   ( Generic )
import           Servant                        ( ServerError(..)
                                                , err404
                                                )
import           Servant.Docs                   ( ToSample(..)
                                                , singleSample
                                                )
import           Text.RawString.QQ              ( r )

import           App
import           Models.DB
import           Utils                          ( prefixToJSON )

import qualified Data.Text                     as T
import qualified Database.Esqueleto.Experimental
                                               as E


-- SIDEBAR

data BlogSidebarData = BlogSidebarData
    { bsdRecent     :: [BlogRecentPostData]
    , bsdArchive    :: [BlogArchiveYearData]
    , bsdTags       :: [BlogTagData]
    , bsdCategories :: [BlogSidebarCategoryData]
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogSidebarData where
    toJSON = prefixToJSON "bsd"

instance ToSample BlogSidebarData where
    toSamples _ = singleSample $ BlogSidebarData
        { bsdRecent     =
            [ BlogRecentPostData "Newest Post"           "newest-post"
            , BlogRecentPostData "Second in line"        "second-in-line"
            , BlogRecentPostData "Oldest in limit query" "oldest-in-limit-query"
            ]
        , bsdArchive    = [ BlogArchiveYearData 2022 8 15
                          , BlogArchiveYearData 2022 1 42
                          , BlogArchiveYearData 2020 6 9001
                          , BlogArchiveYearData 2012 5 1
                          ]
        , bsdTags       = [ BlogTagData "Augment"    10
                          , BlogTagData "Haskell"    42
                          , BlogTagData "Purescript" 9001
                          , BlogTagData "Wordpress"  1
                          ]
        , bsdCategories =
            [ BlogSidebarCategoryData "Guides"     "guides"     9
            , BlogSidebarCategoryData "Home Lab"   "home-lab"   2
            , BlogSidebarCategoryData "Rave Saber" "rave-saber" 121
            ]
        }

data BlogRecentPostData = BlogRecentPostData
    { brpdTitle :: Text
    , brpdSlug  :: Text
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogRecentPostData where
    toJSON = prefixToJSON "brpd"

data BlogArchiveYearData = BlogArchiveYearData
    { baydYear  :: Int
    , baydMonth :: Int
    , baydCount :: Int
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogArchiveYearData where
    toJSON = prefixToJSON "bayd"

data BlogTagData = BlogTagData
    { btdTag   :: Text
    , btdCount :: Int
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogTagData where
    toJSON = prefixToJSON "btd"

data BlogSidebarCategoryData = BlogSidebarCategoryData
    { bscdTitle :: Text
    , bscdSlug  :: Text
    , bscdCount :: Int
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogSidebarCategoryData where
    toJSON = prefixToJSON "bscd"


-- | TODO: This should be cached in a TVar that lives in Config type.
-- Regenerate it on bootup, after creating or updating a Post. Expose
-- effect typeclass for fetching & generating it.
getBlogSidebarData :: MonadIO m => SqlPersistT m BlogSidebarData
getBlogSidebarData = do
    let mkRecent (Entity _ p) =
            BlogRecentPostData (blogPostTitle p) (blogPostSlug p)
    bsdRecent <- map mkRecent <$> selectList
        [BlogPostPublishedAt !=. Nothing]
        [Desc BlogPostPublishedAt, LimitTo 5]
    let archiveQuery = [r|
            SELECT
                DATE_PART('year', published_at) AS year,
                DATE_PART('month', published_at) AS month,
                COUNT(*)
            FROM blog_post
            GROUP BY year, month
            |]
        mkArchive (Single y, Single m, Single c) = BlogArchiveYearData y m c
    bsdArchive <- map mkArchive <$> rawSql archiveQuery []
    let tagQuery = [r|
            SELECT
                TRIM(FROM UNNEST(STRING_TO_ARRAY(tags, ','))) AS tag,
                COUNT(*)
            FROM blog_post
            GROUP BY tag
            ORDER BY tag ASC
            |]
        mkTag (Single t, Single c) = BlogTagData t c
    bsdTags <- map mkTag <$> rawSql tagQuery []
    let categoryQuery = E.select $ do
            (_ E.:& category) <-
                E.from
                $             E.table @BlogPost
                `E.InnerJoin` E.table @BlogCategory
                `E.on`        (\(post E.:& category) ->
                                  (post E.^. BlogPostCategoryId)
                                      E.==. (category E.^. BlogCategoryId)
                              )
            E.groupBy
                ( category E.^. BlogCategoryTitle
                , category E.^. BlogCategorySlug
                )
            E.orderBy [E.asc $ category E.^. BlogCategoryTitle]
            E.having $ E.countRows @Int E.>. E.val 0
            return
                ( category E.^. BlogCategoryTitle
                , category E.^. BlogCategorySlug
                , E.countRows
                )
        mkCategory (E.Value t, E.Value s, E.Value c) =
            BlogSidebarCategoryData t s c
    bsdCategories <- map mkCategory <$> categoryQuery
    return BlogSidebarData { .. }


-- CATEGORY

data BlogCategoryData = BlogCategoryData
    { bcdTitle :: Text
    , bcdSlug  :: Text
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogCategoryData where
    toJSON = prefixToJSON "bcd"

mkCategoryData :: BlogCategory -> BlogCategoryData
mkCategoryData BlogCategory {..} = BlogCategoryData
    { bcdTitle = blogCategoryTitle
    , bcdSlug  = blogCategorySlug
    }


-- LIST

data BlogPostList = BlogPostList
    { bplPosts   :: [BlogPostListData]
    , bplSidebar :: BlogSidebarData
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogPostList where
    toJSON = prefixToJSON "bpl"

instance ToSample BlogPostList where
    toSamples _ = singleSample $ BlogPostList
        (map snd $ toSamples $ Proxy @BlogPostListData)
        (head $ map snd $ toSamples $ Proxy @BlogSidebarData)

data BlogPostListData = BlogPostListData
    { bpldTitle       :: Text
    , bpldSlug        :: Text
    , bpldTags        :: [Text]
    , bpldCategory    :: BlogCategoryData
    , bpldCreatedAt   :: UTCTime
    , bpldUpdatedAt   :: UTCTime
    , bpldPublishedAt :: UTCTime
    , bpldDescription :: Text
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogPostListData where
    toJSON = prefixToJSON "bpld"

instance ToSample BlogPostListData where
    toSamples _ = singleSample $ BlogPostListData
        "Some Post Title"
        "some-post-title"
        ["tag1", "tag two"]
        (BlogCategoryData "My Category" "my-category")
        (UTCTime (fromGregorian 2022 04 20) 0)
        (UTCTime (fromGregorian 2022 04 20) 0)
        (UTCTime (fromGregorian 2022 04 20) 0)
        "Custom description text for the post or the first paragraph."

mkPostData :: Entity BlogPost -> Entity BlogCategory -> BlogPostListData
mkPostData (Entity _ BlogPost {..}) (Entity _ category) = BlogPostListData
    { bpldTitle       = blogPostTitle
    , bpldSlug        = blogPostSlug
    , bpldTags        = mkTagList blogPostTags
    , bpldCategory    = mkCategoryData category
    , bpldCreatedAt   = blogPostCreatedAt
    , bpldUpdatedAt   = blogPostUpdatedAt
    , bpldPublishedAt = fromJust blogPostPublishedAt
    , bpldDescription = blogPostDescription
    }


-- | All Blog Posts
getBlogPosts :: DB m => m BlogPostList
getBlogPosts = runDB . getBlogPostList $ \post ->
    E.not_ $ E.isNothing $ post E.^. BlogPostPublishedAt

-- | Blog Posts by Year-Month
getBlogPostsArchive :: DB m => Integer -> Int -> m BlogPostList
getBlogPostsArchive year month =
    let
        firstDayOfMonth     = fromGregorian year month 1
        firstDayOfNextMonth = addDays 1
            $ fromGregorian year month (gregorianMonthLength year month)
    in
        runDB . getBlogPostList $ \post ->
            (post E.^. BlogPostPublishedAt E.>=. E.just
                    (E.val (UTCTime firstDayOfMonth 0))
                )
                E.&&. (post E.^. BlogPostPublishedAt E.<=. E.just
                          (E.val (UTCTime firstDayOfNextMonth 0))
                      )

-- | Blog Posts with given slugified tag.
getBlogPostsForTag :: DB m => Text -> m BlogPostList
getBlogPostsForTag tag =
    let cleanedTag = T.replace " " "-" $ T.toLower $ T.strip tag
    in  runDB $ do
            bplPosts <- map (uncurry mkPostData) <$> rawSql
                [r|
                    SELECT ??, ??
                    FROM blog_post
                    JOIN blog_category ON category_id=blog_category.id
                    WHERE
                        ? IN (
                            SELECT ((REGEXP_REPLACE(LOWER(TRIM(FROM tag)),' ', '-')))
                            FROM UNNEST(STRING_TO_ARRAY(tags, ',')) AS tag
                        ) AND
                        published_at IS NOT NULL

                |]
                [toPersistValue cleanedTag]
            bplSidebar <- getBlogSidebarData
            return BlogPostList { .. }

-- | Blog Posts with given BlogCategory slug.
getBlogPostsForCategory
    :: (ThrowsError m, DB m, Monad m) => Text -> m BlogPostList
getBlogPostsForCategory categorySlug =
    runDB (getBy $ UniqueBlogCategory categorySlug) >>= \case
        Nothing -> serverError err404
            { errBody = "getBlogPostsForCategory: Category not found"
            }
        Just (Entity categoryId _) -> runDB . getBlogPostList $ \post ->
            post E.^. BlogPostCategoryId E.==. E.val categoryId


-- | Generic data fetcher for a BlogPost list route with an arbitrary set
-- of filters.
getBlogPostList
    :: MonadIO m
    => (E.SqlExpr (Entity BlogPost) -> E.SqlExpr (E.Value Bool))
    -> SqlPersistT m BlogPostList
getBlogPostList filters = do
    bplPosts <- fmap (map $ uncurry mkPostData) $ E.select $ do
        (post E.:& category) <-
            E.from
            $             E.table @BlogPost
            `E.InnerJoin` E.table @BlogCategory
            `E.on`        (\(post E.:& category) ->
                              (post E.^. BlogPostCategoryId)
                                  E.==. (category E.^. BlogCategoryId)
                          )
        E.where_ $ filters post
        E.orderBy [E.desc $ post E.^. BlogPostPublishedAt]
        return (post, category)
    bplSidebar <- getBlogSidebarData
    return BlogPostList { .. }


-- DETAILS

data BlogPostDetails = BlogPostDetails
    { bpdTitle       :: Text
    , bpdSlug        :: Text
    , bpdCreatedAt   :: UTCTime
    , bpdUpdatedAt   :: UTCTime
    , bpdPublishedAt :: UTCTime
    , bpdTags        :: [Text]
    , bpdCategory    :: BlogCategoryData
    , bpdContent     :: Text
    , bpdSidebar     :: BlogSidebarData
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogPostDetails where
    toJSON = prefixToJSON "bpd"

instance ToSample BlogPostDetails where
    toSamples _ = singleSample $ BlogPostDetails
        "I am the title"
        "i-am-the-title"
        (UTCTime (fromGregorian 2022 04 20) 0)
        (UTCTime (fromGregorian 2022 04 20) 0)
        (UTCTime (fromGregorian 2022 04 20) 0)
        ["Haskell", "PHP", "Package Management"]
        (BlogCategoryData "Guides" "guides")
        "I am the post's content"
        (head $ map snd $ toSamples $ Proxy @BlogSidebarData)


getBlogPost :: (ThrowsError m, DB m, Monad m) => Text -> m BlogPostDetails
getBlogPost slug = runDB (getBy (UniqueBlogPost slug)) >>= \case
    Nothing -> serverError err404 { errBody = "getBlogPost: Post not found" }
    Just (Entity _ BlogPost {..}) -> case blogPostPublishedAt of
        Nothing ->
            serverError err404 { errBody = "getBlogPost: Post not found" }
        Just bpdPublishedAt -> runDB $ do
            bpdSidebar <- getBlogSidebarData
            category   <- fromJust <$> get blogPostCategoryId
            return BlogPostDetails { bpdTitle     = blogPostTitle
                                   , bpdSlug      = blogPostSlug
                                   , bpdCreatedAt = blogPostCreatedAt
                                   , bpdUpdatedAt = blogPostUpdatedAt
                                   , bpdTags      = mkTagList blogPostTags
                                   , bpdCategory  = mkCategoryData category
                                   , bpdContent   = blogPostContent
                                   , ..
                                   }


-- UTILS

mkTagList :: Text -> [Text]
mkTagList = filter (not . T.null) . map T.strip . T.splitOn ","
