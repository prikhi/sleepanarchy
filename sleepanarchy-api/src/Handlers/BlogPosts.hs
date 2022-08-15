{-# LANGUAGE RecordWildCards #-}
module Handlers.BlogPosts where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Aeson                     ( ToJSON(..) )
import           Data.Maybe                     ( fromJust )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime(..)
                                                , fromGregorian
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


-- SIDEBAR

data BlogSidebarData = BlogSidebarData
    { bsdRecent  :: [BlogRecentPostData]
    , bsdArchive :: [BlogArchiveYearData]
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogSidebarData where
    toJSON = prefixToJSON "bsd"

instance ToSample BlogSidebarData where
    toSamples _ = singleSample $ BlogSidebarData
        { bsdRecent  =
            [ BlogRecentPostData "Newest Post"           "newest-post"
            , BlogRecentPostData "Second in line"        "second-in-line"
            , BlogRecentPostData "Oldest in limit query" "oldest-in-limit-query"
            ]
        , bsdArchive = [ BlogArchiveYearData 2022 8 15
                       , BlogArchiveYearData 2022 1 42
                       , BlogArchiveYearData 2020 6 9001
                       , BlogArchiveYearData 2012 5 1
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
                DATE_PART('month', published_at) as month,
                COUNT(*)
                FROM blog_post
                GROUP BY year, month
            |]
        mkArchive (Single y, Single m, Single c) = BlogArchiveYearData y m c
    bsdArchive <- map mkArchive <$> rawSql archiveQuery []
    return BlogSidebarData { .. }


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
        (UTCTime (fromGregorian 2022 04 20) 0)
        (UTCTime (fromGregorian 2022 04 20) 0)
        (UTCTime (fromGregorian 2022 04 20) 0)
        "Custom description text for the post or the first paragraph."


getBlogPosts :: DB m => m BlogPostList
getBlogPosts = runDB $ do
    bplPosts <- map mkPostData <$> selectList
        [BlogPostPublishedAt !=. Nothing]
        [Desc BlogPostPublishedAt]
    bplSidebar <- getBlogSidebarData
    return BlogPostList { .. }
  where
    mkPostData :: Entity BlogPost -> BlogPostListData
    mkPostData (Entity _ BlogPost {..}) = BlogPostListData
        { bpldTitle       = blogPostTitle
        , bpldSlug        = blogPostSlug
        , bpldCreatedAt   = blogPostCreatedAt
        , bpldUpdatedAt   = blogPostUpdatedAt
        , bpldPublishedAt = fromJust blogPostPublishedAt
        , bpldDescription = blogPostDescription
        }


-- DETAILS

data BlogPostDetails = BlogPostDetails
    { bpdTitle       :: Text
    , bpdSlug        :: Text
    , bpdCreatedAt   :: UTCTime
    , bpdUpdatedAt   :: UTCTime
    , bpdPublishedAt :: UTCTime
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
        "I am the post's content"
        (head $ map snd $ toSamples $ Proxy @BlogSidebarData)


getBlogPost :: (ThrowsError m, DB m, Monad m) => Text -> m BlogPostDetails
getBlogPost slug = runDB (getBy (UniqueBlogPost slug)) >>= \case
    Nothing -> serverError err404 { errBody = "getBlogPost: Post not found" }
    Just (Entity _ BlogPost {..}) -> case blogPostPublishedAt of
        Nothing ->
            serverError err404 { errBody = "getBlogPost: Post not found" }
        Just bpdPublishedAt -> do
            bpdSidebar <- runDB getBlogSidebarData
            return BlogPostDetails { bpdTitle     = blogPostTitle
                                   , bpdSlug      = blogPostSlug
                                   , bpdCreatedAt = blogPostCreatedAt
                                   , bpdUpdatedAt = blogPostUpdatedAt
                                   , bpdContent   = blogPostContent
                                   , ..
                                   }
