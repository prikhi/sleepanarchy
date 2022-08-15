{-# LANGUAGE RecordWildCards #-}
module Handlers.Admin where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.Char                      ( isAlphaNum )
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.Text                      ( Text )
import           Data.Time                      ( getCurrentTime )
import           Database.Persist.Sql           ( SqlPersistT
                                                , insertUnique
                                                )
import           GHC.Generics                   ( Generic )
import           Servant.Docs                   ( ToSample(..) )

import           App                            ( DB(..) )
import           Models.DB
import           Utils                          ( prefixParseJSON
                                                , prefixToJSON
                                                )

import qualified Data.Text                     as T


data NewBlogPost = NewBlogPost
    { nbpTitle       :: Text
    , nbpSlug        :: Maybe Text
    , nbpDescription :: Maybe Text
    , nbpContent     :: Text
    , nbpPublish     :: Bool
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON NewBlogPost where
    toJSON = prefixToJSON "nbp"

instance FromJSON NewBlogPost where
    parseJSON = prefixParseJSON "nbp"

instance ToSample NewBlogPost where
    toSamples _ =
        [ ( "Auto-generated description & slug"
          , NewBlogPost "My Post's Title"
                        Nothing
                        Nothing
                        "The markdown\n\n---\n\ncontent of the post."
                        False
          )
        , ( "Custom description & slug"
          , NewBlogPost "My Post's Title"
                        (Just "a-custom-slug")
                        (Just "customized description text")
                        "The body of the post"
                        True
          )
        ]

-- TODO: validation - non-empty title & slug, slug is unique
createBlogPost :: (MonadIO m, DB m) => UserId -> NewBlogPost -> m BlogPostId
createBlogPost uid NewBlogPost {..} = do
    let slug        = fromMaybe (slugify nbpTitle) nbpSlug
        description = fromMaybe (mkDescription nbpContent) nbpDescription
    now <- liftIO getCurrentTime
    let newPost = BlogPost
            { blogPostTitle       = nbpTitle
            , blogPostSlug        = slug
            , blogPostDescription = description
            , blogPostContent     = nbpContent
            , blogPostCreatedAt   = now
            , blogPostUpdatedAt   = now
            , blogPostPublishedAt = if nbpPublish then Just now else Nothing
            , blogPostAuthorId    = uid
            }
    runDB $ do
        result <- insertUnique newPost
        maybe (incrementSlugAndInsert newPost 1) return result
  where
    slugify :: Text -> Text
    slugify =
        T.intercalate "-"
            . filter (not . T.null)
            . T.words
            . T.toLower
            . replaceInvalid
    replaceInvalid :: Text -> Text
    replaceInvalid =
        T.map $ \c ->
            if any ($ c) [isAlphaNum, (== '-'), (== '_')] then c else ' '
    mkDescription :: Text -> Text
    mkDescription content = fromMaybe content . listToMaybe $ T.lines content
    incrementSlugAndInsert :: BlogPost -> Integer -> SqlPersistT IO BlogPostId
    incrementSlugAndInsert post ix =
        insertUnique
                (post
                    { blogPostSlug = blogPostSlug post <> "-" <> T.pack
                                         (show ix)
                    }
                )
            >>= \case
                    Nothing     -> incrementSlugAndInsert post (ix + 1)
                    Just postId -> return postId
