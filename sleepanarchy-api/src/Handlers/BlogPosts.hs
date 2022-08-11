{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonoLocalBinds #-}
module Handlers.BlogPosts where

import           Data.Aeson                     ( ToJSON(..) )
import           Data.Maybe                     ( fromJust )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime(..)
                                                , fromGregorian
                                                )
import           Database.Persist
import           GHC.Generics                   ( Generic )
import           Servant.Docs                   ( ToSample(..)
                                                , singleSample
                                                )

import           App
import           Models.DB
import           Utils                          ( prefixToJSON )


newtype BlogPostList = BlogPostList
    { bplPosts :: [BlogPostData]
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogPostList where
    toJSON = prefixToJSON "bpl"

instance ToSample BlogPostList where
    toSamples _ =
        singleSample $ BlogPostList $ map snd $ toSamples $ Proxy @BlogPostData


data BlogPostData = BlogPostData
    { bpdTitle       :: Text
    , bpdSlug        :: Text
    , bpdCreatedAt   :: UTCTime
    , bpdUpdatedAt   :: UTCTime
    , bpdPublishedAt :: UTCTime
    , bpdDescription :: Text
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogPostData where
    toJSON = prefixToJSON "bpd"

instance ToSample BlogPostData where
    toSamples _ = singleSample $ BlogPostData
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
    mkPostData :: Entity BlogPost -> BlogPostData
    mkPostData (Entity _ BlogPost {..}) = BlogPostData
        { bpdTitle       = blogPostTitle
        , bpdSlug        = blogPostSlug
        , bpdCreatedAt   = blogPostCreatedAt
        , bpdUpdatedAt   = blogPostUpdatedAt
        , bpdPublishedAt = fromJust blogPostPublishedAt
        , bpdDescription = blogPostDescription
        }
