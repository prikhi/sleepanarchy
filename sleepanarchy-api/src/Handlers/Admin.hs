{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Admin where

import Control.Exception.Safe (throwM)
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (String)
    , omitNothingFields
    , withText
    )
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Either (fromRight)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (..), fromGregorian, getCurrentTime)
import Database.Persist.Sql (SqlPersistT, fromBackendKey, insertUnique)
import GHC.Generics (Generic)
import Network.Mime (defaultMimeLookup)
import Servant (NoContent (..), err422, errBody)
import Servant.Docs (ToSample (..), singleSample)
import Servant.Server (err403, err404)
import System.FilePath (addTrailingPathSeparator, splitExtension)

import App
    ( Cache (..)
    , DB (..)
    , DBThrows (..)
    , Media (..)
    , ThrowsError (..)
    , foldersToSubPath
    , fromMediaSubPath
    )
import Models.DB
import Models.Utils (slugify)
import Utils (prefixParseJSON, prefixToJSON, prefixToJSONWith, renderMarkdown)

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.List qualified as L
import Data.Text qualified as T
import Database.Esqueleto.Experimental qualified as E
import Database.Persist.Sql qualified as P


-- TODO: Split into Admin.BlogPosts & Admin.Media modules.

-- MEDIA LIST

-- | Media Directory Listing
data AdminMediaList = AdminMediaList
    { amlBasePath :: FilePath
    -- ^ Requested path for the listing.
    , amlContents :: [AdminMediaItem]
    -- ^ Path's contents.
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON AdminMediaList where
    toJSON = prefixToJSON "aml"


instance ToSample AdminMediaList where
    toSamples _ =
        singleSample
            AdminMediaList
                { amlBasePath = "/screenshots"
                , amlContents =
                    [ AdminMediaItem "desktop" Directory
                    , AdminMediaItem "2021-04-20T04:20:00Z.png" Image
                    , AdminMediaItem "postman.mkv" Video
                    ]
                }


-- | A Media Item in a Listing.
data AdminMediaItem = AdminMediaItem
    { amiName :: FilePath
    -- ^ Item's Name
    , amiFileType :: FileType
    -- ^ File or Directory type, with special cases for specific extensions
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON AdminMediaItem where
    toJSON = prefixToJSON "ami"


-- | Enum of different file types, parsed from the file's extension / mime
-- information.
data FileType
    = Image
    | Video
    | Audio
    | Text
    | Directory
    | Other
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON FileType


-- | Given some media directory sub-path, list the files & folders within
-- that path.
listMediaDirectory
    :: (Media m, ThrowsError m, Monad m)
    => UserId
    -> [FilePath]
    -> m AdminMediaList
listMediaDirectory _ folders = do
    let subPath = foldersToSubPath folders
        rawSubPath = fromMediaSubPath subPath
    dirExists <- subDirectoryExists subPath
    unless dirExists $ serverError err404
    contents <- subDirectoryContents subPath
    processed <- forM (L.sort contents) $ \fp -> do
        let subPathFile = foldersToSubPath [rawSubPath, fp]
        isDirectory <- subDirectoryExists subPathFile
        let fileType = if isDirectory then Directory else parseFileType fp
        return $ AdminMediaItem fp fileType
    let amlContents =
            uncurry (<>) $ L.partition ((== Directory) . amiFileType) processed
        amlBasePath =
            if rawSubPath == "."
                then "/"
                else "/" <> addTrailingPathSeparator rawSubPath
    return AdminMediaList {..}
  where
    parseFileType :: FilePath -> FileType
    parseFileType fp =
        case listToMaybe $ BC.split '/' $ defaultMimeLookup $ T.pack fp of
            Just "image" -> Image
            Just "video" -> Video
            Just "audio" -> Audio
            Just "text" -> Text
            _ -> Other


-- | Create a new directory at the given path.
--
-- Throws a 422 if an existing file conflicts with the desired directory
-- name.
createMediaDirectory
    :: (Media m, ThrowsError m, DB m, Monad m)
    => UserId
    -> [FilePath]
    -> m NoContent
createMediaDirectory uid folders = do
    runDB (P.get uid) >>= maybe (serverError err403) (const $ return ())
    let subPath = foldersToSubPath folders
    fileExists <- subFileExists subPath
    when fileExists $
        serverError
            err422
                { errBody = "File with desired folder name already exists."
                }
    createSubDirectory subPath
    return NoContent


-- | A File to upload.
data MediaUpload = MediaUpload
    { muPath :: [FilePath]
    -- ^ Directory path where the file should be placed.
    , muName :: FilePath
    -- ^ Name of the file
    , muData :: Base64Text
    -- ^ Contents of the file.
    --
    -- Note: this is encoded/decoded as Base64 in JSON, but available as
    -- a raw ByteString in Haskell.
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToSample MediaUpload where
    toSamples _ =
        singleSample
            MediaUpload
                { muPath = ["screenshots", "desktop"]
                , muName = "my-vim-setup.png"
                , muData = Base64Text "some image file data would be here"
                }


instance ToJSON MediaUpload where
    toJSON = prefixToJSON "mu"
instance FromJSON MediaUpload where
    parseJSON = prefixParseJSON "mu"


-- | Name of a newly uploaded file.
newtype FileName = FileName
    { fromFileName :: FilePath
    }
    deriving (Show, Read, Eq, Ord)
    deriving newtype (ToJSON)


instance ToSample FileName where
    toSamples _ = singleSample $ FileName "my-file.png"


-- | Binary text data decoded from Base64.
--
-- Note: The base64 representation is only present over-the-wire & is
-- completely handled by JSON instances.
newtype Base64Text = Base64Text
    { fromBase64Text :: BS.ByteString
    -- ^ Binary data encoded in UTF. TODO: could swap to bytestring.
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON Base64Text where
    toJSON = String . encodeBase64 . fromBase64Text
instance FromJSON Base64Text where
    parseJSON =
        withText "Base64Text" $
            either (fail . T.unpack) (return . Base64Text)
                . decodeBase64
                . encodeUtf8


-- | Upload a file, creating any necessary sub-directories & adding
-- a suffix to the filename if a file with the given name already exists.
uploadMediaFile
    :: (Media m, DB m, ThrowsError m, Monad m)
    => UserId
    -> MediaUpload
    -> m FileName
uploadMediaFile uid MediaUpload {..} = do
    runDB (P.get uid) >>= maybe (serverError err403) (const $ return ())
    FileName <$> saveFile 0
  where
    saveFile :: (Media m, Monad m) => Int -> m FilePath
    saveFile = \case
        0 -> do
            let subPath = foldersToSubPath $ muPath <> [muName]
            exists <- subPathExists subPath
            if exists
                then saveFile 1
                else do
                    writeFileToSubPath subPath $ fromBase64Text muData
                    return $ fromMediaSubPath subPath
        ix -> do
            let (base, ext) = splitExtension muName
                subPath =
                    foldersToSubPath $
                        muPath
                            <> [base <> "-" <> showWithPadding ix <> ext]
            exists <- subPathExists subPath
            if exists
                then saveFile (ix + 1)
                else do
                    writeFileToSubPath subPath $ fromBase64Text muData
                    return $ fromMediaSubPath subPath
    showWithPadding :: Int -> String
    showWithPadding ix
        | ix < 10 = "00" <> show ix
        | ix < 100 = "0" <> show ix
        | otherwise = show ix


-- BLOG POST LIST

newtype AdminBlogPostList = AdminBlogPostList {abplPosts :: [BlogPostListItem]} deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON AdminBlogPostList where
    toJSON = prefixToJSON "abpl"


instance ToSample AdminBlogPostList where
    toSamples _ =
        singleSample $
            AdminBlogPostList
                [ BlogPostListItem
                    { bpliId = fromBackendKey 42
                    , bpliTitle = "Title 1"
                    , bpliCategory = "Example Category"
                    , bpliCreated = UTCTime (fromGregorian 2022 04 20) 0
                    , bpliPublished = Nothing
                    }
                , BlogPostListItem
                    { bpliId = fromBackendKey 9001
                    , bpliTitle = "Title 2"
                    , bpliCategory = "Different Category"
                    , bpliCreated = UTCTime (fromGregorian 2021 04 20) 0
                    , bpliPublished = Just (UTCTime (fromGregorian 2021 04 20) 0)
                    }
                ]


data BlogPostListItem = BlogPostListItem
    { bpliId :: BlogPostId
    , bpliTitle :: Text
    , bpliCategory :: Text
    , bpliCreated :: UTCTime
    , bpliPublished :: Maybe UTCTime
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON BlogPostListItem where
    toJSON = prefixToJSON "bpli"


getBlogPostsAdmin :: (MonadIO m, DB m) => UserId -> m AdminBlogPostList
getBlogPostsAdmin _ =
    fmap (AdminBlogPostList . fmap mkPost) . runDB . E.select $ do
        (post E.:& category) <-
            E.from $
                E.table @BlogPost
                    `E.InnerJoin` E.table @BlogCategory
                        `E.on` ( \(post E.:& category) ->
                                    (post E.^. BlogPostCategoryId)
                                        E.==. (category E.^. BlogCategoryId)
                               )
        E.orderBy [E.desc $ post E.^. BlogPostCreatedAt]
        return (post, category)
  where
    mkPost :: (E.Entity BlogPost, E.Entity BlogCategory) -> BlogPostListItem
    mkPost (E.Entity pId BlogPost {..}, E.Entity _ category) =
        BlogPostListItem
            { bpliId = pId
            , bpliTitle = blogPostTitle
            , bpliCategory = blogCategoryTitle category
            , bpliCreated = blogPostCreatedAt
            , bpliPublished = blogPostPublishedAt
            }


-- BLOG POST GET

data AdminBlogPost = AdminBlogPost
    { abpId :: BlogPostId
    , abpTitle :: Text
    , abpSlug :: Text
    , abpDescription :: Text
    , abpContent :: Text
    , abpTags :: Text
    , abpCategory :: BlogCategoryId
    , abpCreatedAt :: UTCTime
    , abpUpdatedAt :: UTCTime
    , abpPublishedAt :: Maybe UTCTime
    , abpCategories :: [AdminBlogCategory]
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON AdminBlogPost where
    toJSON = prefixToJSON "abp"


instance ToSample AdminBlogPost where
    toSamples _ =
        singleSample
            AdminBlogPost
                { abpId = P.fromBackendKey 42
                , abpTitle = "Post Title"
                , abpSlug = "post-title"
                , abpDescription = "Description of Post, custom or auto-generated"
                , abpContent = "Full content of the Post"
                , abpTags = "comma, separated, tag list"
                , abpCategory = P.fromBackendKey 3
                , abpCreatedAt = UTCTime (fromGregorian 2022 01 10) 0
                , abpUpdatedAt = UTCTime (fromGregorian 2022 02 15) 0
                , abpPublishedAt = Just $ UTCTime (fromGregorian 2022 02 15) 0
                , abpCategories =
                    [ AdminBlogCategory "Category" $ P.fromBackendKey 3
                    , AdminBlogCategory "List" $ P.fromBackendKey 8
                    , AdminBlogCategory "That is Alphabetical" $ P.fromBackendKey 1
                    ]
                }


data AdminBlogCategory = AdminBlogCategory
    { abcTitle :: Text
    , abcId :: BlogCategoryId
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON AdminBlogCategory where
    toJSON = prefixToJSON "abc"


instance ToSample AdminBlogCategory where
    toSamples _ =
        map
            ("",)
            [ AdminBlogCategory "Category" $ P.fromBackendKey 3
            , AdminBlogCategory "List" $ P.fromBackendKey 8
            , AdminBlogCategory "That is Alphabetical" $ P.fromBackendKey 1
            ]


getBlogPostAdmin :: DBThrows m => UserId -> BlogPostId -> m AdminBlogPost
getBlogPostAdmin _ pId =
    runDBThrow $
        P.get pId >>= \case
            Nothing -> throwM err404
            Just BlogPost {..} -> do
                categories <-
                    map mkBlogCategory
                        <$> P.selectList [] [P.Asc BlogCategoryTitle]
                return
                    AdminBlogPost
                        { abpId = pId
                        , abpTitle = blogPostTitle
                        , abpSlug = blogPostSlug
                        , abpDescription = blogPostDescription
                        , abpContent = blogPostContent
                        , abpTags = blogPostTags
                        , abpCategory = blogPostCategoryId
                        , abpCreatedAt = blogPostCreatedAt
                        , abpUpdatedAt = blogPostUpdatedAt
                        , abpPublishedAt = blogPostPublishedAt
                        , abpCategories = categories
                        }


getBlogCategoriesAdmin :: DB m => UserId -> m [AdminBlogCategory]
getBlogCategoriesAdmin _ =
    runDB $ map mkBlogCategory <$> P.selectList [] [P.Asc BlogCategoryTitle]


mkBlogCategory :: P.Entity BlogCategory -> AdminBlogCategory
mkBlogCategory (P.Entity cId c) = AdminBlogCategory (blogCategoryTitle c) cId


-- BLOG POST UPDATE

data AdminBlogPostUpdate = AdminBlogPostUpdate
    { abpuTitle :: Maybe Text
    , abpuSlug :: Maybe Text
    -- ^ Set to 'Just ""' to autogenerate from provided or existing title.
    , abpuContent :: Maybe Text
    , abpuDescription :: Maybe Text
    , abpuTags :: Maybe Text
    , abpuPublished :: Maybe Bool
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance FromJSON AdminBlogPostUpdate where
    parseJSON = prefixParseJSON "abpu"


instance ToJSON AdminBlogPostUpdate where
    toJSON = prefixToJSONWith "abpu" (\o -> o {omitNothingFields = True})


instance ToSample AdminBlogPostUpdate where
    toSamples _ =
        [
            ( "Full Update"
            , AdminBlogPostUpdate
                { abpuTitle = Just "New Title"
                , abpuSlug = Just "new-title"
                , abpuContent = Just "New Content\n\n---\n\nIn Markdown"
                , abpuDescription = Just "New short description for list views"
                , abpuTags = Just "New Tags, and Stuffs"
                , abpuPublished = Just True
                }
            )
        ,
            ( "Partial Update"
            , AdminBlogPostUpdate
                { abpuTitle = Nothing
                , abpuSlug = Nothing
                , abpuContent = Just "New Content\n\n---\n\nIn Markdown"
                , abpuDescription = Nothing
                , abpuTags = Just "New Tags, and, Stuff"
                , abpuPublished = Nothing
                }
            )
        ,
            ( "Publish"
            , AdminBlogPostUpdate
                { abpuTitle = Nothing
                , abpuSlug = Nothing
                , abpuContent = Nothing
                , abpuDescription = Nothing
                , abpuTags = Nothing
                , abpuPublished = Just True
                }
            )
        ,
            ( "Unpublish"
            , AdminBlogPostUpdate
                { abpuTitle = Nothing
                , abpuSlug = Nothing
                , abpuContent = Nothing
                , abpuDescription = Nothing
                , abpuTags = Nothing
                , abpuPublished = Just False
                }
            )
        ,
            ( "Autogenerate Slug"
            , AdminBlogPostUpdate
                { abpuTitle = Just "new title to generate slug from"
                , abpuSlug = Just ""
                , abpuContent = Just "New Content\n\n---\n\nIn Markdown"
                , abpuDescription = Nothing
                , abpuTags = Just "New Tags, and, Stuff"
                , abpuPublished = Nothing
                }
            )
        ]


-- | TODO: throw error when rendering markdown fails. handle error in
-- client.
updateBlogPost
    :: (DBThrows m, Cache m, Monad m)
    => UserId
    -> BlogPostId
    -> AdminBlogPostUpdate
    -> m NoContent
updateBlogPost _ pId AdminBlogPostUpdate {..} = do
    runDBThrow $ do
        now <- liftIO getCurrentTime
        post <- P.get pId >>= maybe (throwM err404) return
        slugUpdate <- forM abpuSlug $ \newSlug ->
            if T.null newSlug
                then case abpuTitle of
                    Just newTitle -> return $ slugify newTitle
                    Nothing -> do
                        return $ slugify $ blogPostTitle post
                else return newSlug
        let publishUpdate =
                abpuPublished >>= \shouldPublish ->
                    case (blogPostPublishedAt post, shouldPublish) of
                        (Nothing, True) -> Just $ Just now
                        (Just _, False) -> Just Nothing
                        (Nothing, False) -> Nothing
                        (Just _, True) -> Nothing
        let updates =
                catMaybes
                    [ (BlogPostTitle P.=.) <$> abpuTitle
                    , (BlogPostSlug P.=.) <$> slugUpdate
                    , (BlogPostContent P.=.) <$> abpuContent
                    , (BlogPostContentHtml P.=.) . renderMarkdownSafe <$> abpuContent
                    , (BlogPostDescription P.=.) <$> abpuDescription
                    , (BlogPostDescriptionHtml P.=.) . renderMarkdownSafe <$> abpuDescription
                    , (BlogPostTags P.=.) <$> abpuTags
                    , (BlogPostPublishedAt P.=.) <$> publishUpdate
                    ]
        unless (null updates) $ do
            P.update pId $ (BlogPostUpdatedAt P.=. now) : updates
    bustBlogSidebarCache
    return NoContent
  where
    renderMarkdownSafe :: Text -> Text
    renderMarkdownSafe s = fromRight s $ renderMarkdown s


-- NEW BLOG POST

data NewBlogPost = NewBlogPost
    { nbpTitle :: Text
    , nbpSlug :: Maybe Text
    , nbpDescription :: Maybe Text
    , nbpContent :: Text
    , nbpTags :: Text
    , nbpPublish :: Bool
    , nbpCategoryId :: BlogCategoryId
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON NewBlogPost where
    toJSON = prefixToJSON "nbp"


instance FromJSON NewBlogPost where
    parseJSON = prefixParseJSON "nbp"


instance ToSample NewBlogPost where
    toSamples _ =
        [
            ( "Auto-generated description & slug"
            , NewBlogPost
                "My Post's Title"
                Nothing
                Nothing
                "The markdown\n\n---\n\ncontent of the post."
                "comma, separated, list of tags"
                False
                (fromBackendKey 9001)
            )
        ,
            ( "Custom description & slug"
            , NewBlogPost
                "My Post's Title"
                (Just "a-custom-slug")
                (Just "customized description text")
                "The body of the post"
                ""
                True
                (fromBackendKey 42)
            )
        ]


-- | TODO: validation:
-- * non-empty title & slug
-- * slug is unique(when specified)
-- * category exists
--
-- TODO: Do we want to keep description auto-generation? Should it be first
-- _paragraph_ of the content instead of the first line? Should we render
-- the content field's markdown into HTML & take the first paragraph from
-- that? Seems simpler to just require a non-empty Description field...
--
-- TODO: throw error if markdown rendering fails. handle error in client.
createBlogPost
    :: (MonadIO m, DB m, Cache m) => UserId -> NewBlogPost -> m BlogPostId
createBlogPost uid NewBlogPost {..} = do
    let slug = fromMaybe (slugify nbpTitle) nbpSlug
        description = fromMaybe (mkDescription nbpContent) nbpDescription
    now <- liftIO getCurrentTime
    let newPost =
            BlogPost
                { blogPostTitle = nbpTitle
                , blogPostSlug = slug
                , blogPostDescription = description
                , blogPostDescriptionHtml = renderMarkdownSafe description
                , blogPostContent = nbpContent
                , blogPostContentHtml = renderMarkdownSafe nbpContent
                , blogPostTags = nbpTags
                , blogPostCreatedAt = now
                , blogPostUpdatedAt = now
                , blogPostPublishedAt = if nbpPublish then Just now else Nothing
                , blogPostAuthorId = uid
                , blogPostCategoryId = nbpCategoryId
                }
    postId <- runDB $ do
        result <- insertUnique newPost
        maybe (incrementSlugAndInsert newPost 1) return result
    bustBlogSidebarCache
    return postId
  where
    mkDescription :: Text -> Text
    mkDescription content = fromMaybe content . listToMaybe $ T.lines content
    incrementSlugAndInsert :: BlogPost -> Integer -> SqlPersistT IO BlogPostId
    incrementSlugAndInsert post ix =
        insertUnique
            ( post
                { blogPostSlug =
                    blogPostSlug post
                        <> "-"
                        <> T.pack
                            (show ix)
                }
            )
            >>= \case
                Nothing -> incrementSlugAndInsert post (ix + 1)
                Just postId -> return postId
    renderMarkdownSafe :: Text -> Text
    renderMarkdownSafe s = fromRight s $ renderMarkdown s
