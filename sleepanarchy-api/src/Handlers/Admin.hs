{-# LANGUAGE RecordWildCards #-}
module Handlers.Admin where

import           Control.Exception.Safe         ( throwM )
import           Control.Monad                  ( forM
                                                , unless
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , omitNothingFields
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , listToMaybe
                                                )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime(..)
                                                , fromGregorian
                                                , getCurrentTime
                                                )
import           Database.Persist.Sql           ( SqlPersistT
                                                , fromBackendKey
                                                , insertUnique
                                                )
import           GHC.Generics                   ( Generic )
import           Servant                        ( NoContent(..) )
import           Servant.Docs                   ( ToSample(..)
                                                , singleSample
                                                )
import           Servant.Server                 ( err404 )

import           App                            ( DB(..)
                                                , DBThrows(..)
                                                )
import           Models.DB
import           Models.Utils                   ( slugify )
import           Utils                          ( prefixParseJSON
                                                , prefixToJSON
                                                , prefixToJSONWith
                                                )

import qualified Data.Text                     as T
import qualified Database.Esqueleto.Experimental
                                               as E
import qualified Database.Persist.Sql          as P


-- BLOG POST LIST

newtype AdminBlogPostList = AdminBlogPostList { abplPosts :: [BlogPostListItem] } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON AdminBlogPostList where
    toJSON = prefixToJSON "abpl"

instance ToSample AdminBlogPostList where
    toSamples _ = singleSample $ AdminBlogPostList
        [ BlogPostListItem { bpliId        = fromBackendKey 42
                           , bpliTitle     = "Title 1"
                           , bpliCategory  = "Example Category"
                           , bpliCreated = UTCTime (fromGregorian 2022 04 20) 0
                           , bpliPublished = Nothing
                           }
        , BlogPostListItem
            { bpliId        = fromBackendKey 9001
            , bpliTitle     = "Title 2"
            , bpliCategory  = "Different Category"
            , bpliCreated   = UTCTime (fromGregorian 2021 04 20) 0
            , bpliPublished = Just (UTCTime (fromGregorian 2021 04 20) 0)
            }
        ]

data BlogPostListItem = BlogPostListItem
    { bpliId        :: BlogPostId
    , bpliTitle     :: Text
    , bpliCategory  :: Text
    , bpliCreated   :: UTCTime
    , bpliPublished :: Maybe UTCTime
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BlogPostListItem where
    toJSON = prefixToJSON "bpli"

getBlogPostsAdmin :: (MonadIO m, DB m) => UserId -> m AdminBlogPostList
getBlogPostsAdmin _ =
    fmap (AdminBlogPostList . fmap mkPost) . runDB . E.select $ do
        (post E.:& category) <-
            E.from
            $             E.table @BlogPost
            `E.InnerJoin` E.table @BlogCategory
            `E.on`        (\(post E.:& category) ->
                              (post E.^. BlogPostCategoryId)
                                  E.==. (category E.^. BlogCategoryId)
                          )
        E.orderBy [E.desc $ post E.^. BlogPostCreatedAt]
        return (post, category)
  where
    mkPost :: (E.Entity BlogPost, E.Entity BlogCategory) -> BlogPostListItem
    mkPost (E.Entity pId BlogPost {..}, E.Entity _ category) = BlogPostListItem
        { bpliId        = pId
        , bpliTitle     = blogPostTitle
        , bpliCategory  = blogCategoryTitle category
        , bpliCreated   = blogPostCreatedAt
        , bpliPublished = blogPostPublishedAt
        }

-- BLOG POST GET

data AdminBlogPost = AdminBlogPost
    { abpId          :: BlogPostId
    , abpTitle       :: Text
    , abpSlug        :: Text
    , abpDescription :: Text
    , abpContent     :: Text
    , abpTags        :: Text
    , abpCategory    :: BlogCategoryId
    , abpCreatedAt   :: UTCTime
    , abpUpdatedAt   :: UTCTime
    , abpPublishedAt :: Maybe UTCTime
    , abpCategories  :: [AdminBlogCategory]
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON AdminBlogPost where
    toJSON = prefixToJSON "abp"

instance ToSample AdminBlogPost where
    toSamples _ = singleSample AdminBlogPost
        { abpId          = P.fromBackendKey 42
        , abpTitle       = "Post Title"
        , abpSlug        = "post-title"
        , abpDescription = "Description of Post, custom or auto-generated"
        , abpContent     = "Full content of the Post"
        , abpTags        = "comma, separated, tag list"
        , abpCategory    = P.fromBackendKey 3
        , abpCreatedAt   = UTCTime (fromGregorian 2022 01 10) 0
        , abpUpdatedAt   = UTCTime (fromGregorian 2022 02 15) 0
        , abpPublishedAt = Just $ UTCTime (fromGregorian 2022 02 15) 0
        , abpCategories  =
            [ AdminBlogCategory "Category" $ P.fromBackendKey 3
            , AdminBlogCategory "List" $ P.fromBackendKey 8
            , AdminBlogCategory "That is Alphabetical" $ P.fromBackendKey 1
            ]
        }

data AdminBlogCategory = AdminBlogCategory
    { abcTitle :: Text
    , abcId    :: BlogCategoryId
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON AdminBlogCategory where
    toJSON = prefixToJSON "abc"

getBlogPostAdmin :: DBThrows m => UserId -> BlogPostId -> m AdminBlogPost
getBlogPostAdmin _ pId = runDBThrow $ P.get pId >>= \case
    Nothing            -> throwM err404
    Just BlogPost {..} -> do
        categories <-
            map
                    (\(P.Entity cId c) ->
                        AdminBlogCategory (blogCategoryTitle c) cId
                    )
                <$> P.selectList [] [P.Desc BlogCategoryTitle]
        return AdminBlogPost { abpId          = pId
                             , abpTitle       = blogPostTitle
                             , abpSlug        = blogPostSlug
                             , abpDescription = blogPostDescription
                             , abpContent     = blogPostContent
                             , abpTags        = blogPostTags
                             , abpCategory    = blogPostCategoryId
                             , abpCreatedAt   = blogPostCreatedAt
                             , abpUpdatedAt   = blogPostUpdatedAt
                             , abpPublishedAt = blogPostPublishedAt
                             , abpCategories  = categories
                             }

-- BLOG POST UPDATE

data AdminBlogPostUpdate = AdminBlogPostUpdate
    { abpuTitle       :: Maybe Text
    , abpuSlug        :: Maybe Text
    -- ^ Set to 'Just ""' to autogenerate from provided or existing title.
    , abpuContent     :: Maybe Text
    , abpuDescription :: Maybe Text
    , abpuTags        :: Maybe Text
    , abpuPublished   :: Maybe Bool
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON AdminBlogPostUpdate where
    parseJSON = prefixParseJSON "abpu"

instance ToJSON AdminBlogPostUpdate where
    toJSON = prefixToJSONWith "abpu" (\o -> o { omitNothingFields = True })

instance ToSample AdminBlogPostUpdate where
    toSamples _ =
        [ ( "Full Update"
          , AdminBlogPostUpdate
              { abpuTitle       = Just "New Title"
              , abpuSlug        = Just "new-title"
              , abpuContent     = Just "New Content\n\n---\n\nIn Markdown"
              , abpuDescription = Just "New short description for list views"
              , abpuTags        = Just "New Tags, and Stuffs"
              , abpuPublished   = Just True
              }
          )
        , ( "Partial Update"
          , AdminBlogPostUpdate
              { abpuTitle       = Nothing
              , abpuSlug        = Nothing
              , abpuContent     = Just "New Content\n\n---\n\nIn Markdown"
              , abpuDescription = Nothing
              , abpuTags        = Just "New Tags, and, Stuff"
              , abpuPublished   = Nothing
              }
          )
        , ( "Publish"
          , AdminBlogPostUpdate { abpuTitle       = Nothing
                                , abpuSlug        = Nothing
                                , abpuContent     = Nothing
                                , abpuDescription = Nothing
                                , abpuTags        = Nothing
                                , abpuPublished   = Just True
                                }
          )
        , ( "Unpublish"
          , AdminBlogPostUpdate { abpuTitle       = Nothing
                                , abpuSlug        = Nothing
                                , abpuContent     = Nothing
                                , abpuDescription = Nothing
                                , abpuTags        = Nothing
                                , abpuPublished   = Just False
                                }
          )
        , ( "Autogenerate Slug"
          , AdminBlogPostUpdate
              { abpuTitle       = Just "new title to generate slug from"
              , abpuSlug        = Just ""
              , abpuContent     = Just "New Content\n\n---\n\nIn Markdown"
              , abpuDescription = Nothing
              , abpuTags        = Just "New Tags, and, Stuff"
              , abpuPublished   = Nothing
              }
          )
        ]

updateBlogPost
    :: DBThrows m => UserId -> BlogPostId -> AdminBlogPostUpdate -> m NoContent
updateBlogPost _ pId AdminBlogPostUpdate {..} = runDBThrow $ do
    now        <- liftIO getCurrentTime
    post       <- P.get pId >>= maybe (throwM err404) return
    slugUpdate <- forM abpuSlug $ \newSlug -> if T.null newSlug
        then case abpuTitle of
            Just newTitle -> return $ slugify newTitle
            Nothing       -> do
                return $ slugify $ blogPostTitle post
        else return newSlug
    let publishUpdate = abpuPublished >>= \shouldPublish ->
            case (blogPostPublishedAt post, shouldPublish) of
                (Nothing, True ) -> Just $ Just now
                (Just _ , False) -> Just Nothing
                (Nothing, False) -> Nothing
                (Just _ , True ) -> Nothing
    let updates = catMaybes
            [ (BlogPostTitle P.=.) <$> abpuTitle
            , (BlogPostSlug P.=.) <$> slugUpdate
            , (BlogPostContent P.=.) <$> abpuContent
            , (BlogPostDescription P.=.) <$> abpuDescription
            , (BlogPostTags P.=.) <$> abpuTags
            , (BlogPostPublishedAt P.=.) <$> publishUpdate
            ]
    unless (null updates)
        $ P.update pId
        $ (BlogPostUpdatedAt P.=. now)
        : updates
    return NoContent


-- NEW BLOG POST

data NewBlogPost = NewBlogPost
    { nbpTitle       :: Text
    , nbpSlug        :: Maybe Text
    , nbpDescription :: Maybe Text
    , nbpContent     :: Text
    , nbpTags        :: Text
    , nbpPublish     :: Bool
    , nbpCategoryId  :: BlogCategoryId
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
                        "comma, separated, list of tags"
                        False
                        (fromBackendKey 9001)
          )
        , ( "Custom description & slug"
          , NewBlogPost "My Post's Title"
                        (Just "a-custom-slug")
                        (Just "customized description text")
                        "The body of the post"
                        ""
                        True
                        (fromBackendKey 42)
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
            , blogPostTags        = nbpTags
            , blogPostCreatedAt   = now
            , blogPostUpdatedAt   = now
            , blogPostPublishedAt = if nbpPublish then Just now else Nothing
            , blogPostAuthorId    = uid
            , blogPostCategoryId  = nbpCategoryId
            }
    runDB $ do
        result <- insertUnique newPost
        maybe (incrementSlugAndInsert newPost 1) return result
  where
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
