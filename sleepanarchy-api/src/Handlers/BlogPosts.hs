{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonoLocalBinds #-}
module Handlers.BlogPosts where

import           Data.Aeson                     ( ToJSON(..) )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime(..)
                                                , fromGregorian
                                                )
import           Database.Persist
import           GHC.Generics                   ( Generic )
import           Servant                        ( ServerError(..)
                                                , err404
                                                )
import           Servant.Docs                   ( ToSample(..)
                                                , singleSample
                                                )

import           App
import           Models.DB
import           Utils                          ( prefixToJSON )


newtype BlogPostList = BlogPostList
    { bplPosts :: [BlogPostListData]
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogPostList where
    toJSON = prefixToJSON "bpl"

instance ToSample BlogPostList where
    toSamples _ =
        singleSample
            $ BlogPostList
            $ map snd
            $ toSamples
            $ Proxy @BlogPostListData


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
getBlogPosts = runDB $ BlogPostList . map mkPostData <$> selectList
    [BlogPostPublishedAt !=. Nothing]
    [Desc BlogPostPublishedAt]
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



data BlogPostDetails = BlogPostDetails
    { bpdTitle     :: Text
    , bpdSlug      :: Text
    , bpdCreatedAt :: UTCTime
    , bpdUpdatedAt :: UTCTime
    , bpdContent   :: Text
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
        "I am the post's content"

getBlogPost :: (ThrowsError m, DB m, Monad m) => Text -> m BlogPostDetails
getBlogPost slug = runDB (getBy (UniqueBlogPost slug)) >>= \case
    Nothing -> serverError err404 { errBody = "getBlogPost: Post not found" }
    Just (Entity _ BlogPost {..})
        | isNothing blogPostPublishedAt -> serverError err404
            { errBody = "getBlogPost: Post not found"
            }
        | otherwise -> return BlogPostDetails { bpdTitle     = blogPostTitle
                                              , bpdSlug      = blogPostSlug
                                              , bpdCreatedAt = blogPostCreatedAt
                                              , bpdUpdatedAt = blogPostUpdatedAt
                                              , bpdContent   = blogPostContent
                                              }
