{-# LANGUAGE RecordWildCards #-}
module Handlers.BlogPosts where

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
import           Database.Persist.Sql           ( rawSql )
import           GHC.Generics                   ( Generic )
import           Servant                        ( ServerError(..)
                                                , err404
                                                )
import           Servant.Docs                   ( ToSample(..)
                                                , singleSample
                                                )
import           Text.RawString.QQ              ( r )

import           App
import           Caches                         ( BlogSidebarData )
import           Models.DB
import           Utils                          ( prefixToJSON )

import qualified Data.Text                     as T
import qualified Database.Esqueleto.Experimental
                                               as E
import           Models.Utils                   ( slugifyTag )


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
getBlogPosts :: (Cache m, DB m, Monad m) => m BlogPostList
getBlogPosts = getBlogPostList
    $ \post -> E.not_ $ E.isNothing $ post E.^. BlogPostPublishedAt

-- | Blog Posts by Year-Month
getBlogPostsArchive
    :: (Cache m, DB m, Monad m) => Integer -> Int -> m BlogPostList
getBlogPostsArchive year month =
    let
        firstDayOfMonth     = fromGregorian year month 1
        firstDayOfNextMonth = addDays 1
            $ fromGregorian year month (gregorianMonthLength year month)
    in
        getBlogPostList $ \post ->
            (post E.^. BlogPostPublishedAt E.>=. E.just
                    (E.val (UTCTime firstDayOfMonth 0))
                )
                E.&&. (post E.^. BlogPostPublishedAt E.<=. E.just
                          (E.val (UTCTime firstDayOfNextMonth 0))
                      )

-- | Blog Posts with given slugified tag.
getBlogPostsForTag :: (Cache m, DB m, Monad m) => Text -> m BlogPostList
getBlogPostsForTag (slugifyTag -> tag) = do
    bplPosts <- fmap (map (uncurry mkPostData)) . runDB $ rawSql
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
        [toPersistValue tag]
    bplSidebar <- getBlogSidebarCache
    return BlogPostList { .. }

-- | Blog Posts with given BlogCategory slug.
getBlogPostsForCategory
    :: (ThrowsError m, DB m, Cache m, Monad m) => Text -> m BlogPostList
getBlogPostsForCategory categorySlug =
    runDB (getBy $ UniqueBlogCategory categorySlug) >>= \case
        Nothing -> serverError err404
            { errBody = "getBlogPostsForCategory: Category not found"
            }
        Just (Entity categoryId _) -> getBlogPostList
            $ \post -> post E.^. BlogPostCategoryId E.==. E.val categoryId


-- | Generic data fetcher for a BlogPost list route with an arbitrary set
-- of filters.
getBlogPostList
    :: (Cache m, DB m, Monad m)
    => (E.SqlExpr (Entity BlogPost) -> E.SqlExpr (E.Value Bool))
    -> m BlogPostList
getBlogPostList filters = do
    bplPosts <- fmap (map $ uncurry mkPostData) . runDB . E.select $ do
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
    bplSidebar <- getBlogSidebarCache
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


getBlogPost
    :: (ThrowsError m, DB m, Monad m, Cache m) => Text -> m BlogPostDetails
getBlogPost slug = runDB (getBy (UniqueBlogPost slug)) >>= \case
    Nothing -> serverError err404 { errBody = "getBlogPost: Post not found" }
    Just (Entity _ BlogPost {..}) -> case blogPostPublishedAt of
        Nothing ->
            serverError err404 { errBody = "getBlogPost: Post not found" }
        Just bpdPublishedAt -> do
            category   <- fmap fromJust . runDB $ get blogPostCategoryId
            bpdSidebar <- getBlogSidebarCache
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
