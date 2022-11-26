module Api.Types where

import Prelude

import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime, Month, Year)
import Data.DateTime.Parsing as DTP
import Data.Either (Either(..), note)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Maybe (Maybe)
import Parsing (parseErrorMessage)

-- GENERAL

-- | A 'DateTime' wrapper representing UTC times from the API, with the
-- appropiate decoding instance.
data ApiDateTime = ApiDateTime DateTime

derive instance eqApiDateTime :: Eq ApiDateTime
derive instance ordApiDateTime :: Ord ApiDateTime

instance decodeDateTime :: DecodeJson ApiDateTime where
  decodeJson json = do
    str <- decodeJson json
    bimap (TypeMismatch <<< parseErrorMessage)
      (\(DTP.FullDateTime dt _) -> ApiDateTime dt) $
      DTP.fromString str

-- BLOG POST

type BlogSidebar =
  { recent :: Array BlogRecentPost
  , archive :: Array BlogArchiveListItem
  , tags :: Array BlogTag
  , categories :: Array BlogSidebarCategory
  }

type BlogRecentPost =
  { title :: String
  , slug :: String
  }

newtype BlogArchiveListItem =
  BlogArchiveListItem
    { year :: Year
    , month :: Month
    , count :: Int
    }

instance decodeJsonBlogArchiveListItem :: DecodeJson BlogArchiveListItem where
  decodeJson json = do
    obj <- decodeJson json
    year <- obj .: "year" >>= parseEnum "Could not parse year"
    month <- obj .: "month" >>= parseEnum "Could not parse month"
    count <- obj .: "count"
    pure $ BlogArchiveListItem { year, month, count }
    where
    parseEnum
      :: forall a. BoundedEnum a => String -> Int -> Either JsonDecodeError a
    parseEnum errMsg i =
      note (TypeMismatch $ errMsg <> ": " <> show i) $ toEnum i

type BlogTag =
  { tag :: String
  , count :: Int
  }

type BlogSidebarCategory =
  { title :: String
  , slug :: String
  , count :: Int
  }

type BlogCategory =
  { title :: String
  , slug :: String
  }

type BlogPostList =
  { posts :: Array BlogPostListItem
  , sidebar :: BlogSidebar
  }

type BlogPostListItem =
  { title :: String
  , description :: String
  , slug :: String
  , createdAt :: ApiDateTime
  , updatedAt :: ApiDateTime
  , publishedAt :: ApiDateTime
  , tags :: Array String
  , category :: BlogCategory
  }

type BlogPostDetails =
  { title :: String
  , content :: String
  , createdAt :: ApiDateTime
  , updatedAt :: ApiDateTime
  , publishedAt :: ApiDateTime
  , tags :: Array String
  , category :: BlogCategory
  , sidebar :: BlogSidebar
  }

-- Links

type RootLinkCategories = Array LinkCategoryMap

newtype LinkCategoryMap =
  LinkCategoryMap
    { category :: String
    , slug :: String
    , children :: Array LinkCategoryMap
    , links :: Array LinkDetails
    }

instance decodeJsonLinkCategoryMap :: DecodeJson LinkCategoryMap where
  decodeJson json = do
    obj <- decodeJson json
    pure $ LinkCategoryMap obj

type LinkDetails =
  { title :: String
  , slug :: String
  , description :: String
  , views :: Int
  }

-- ADMIN BLOG POST

type AdminBlogPostList =
  { posts :: Array AdminBlogPostListItem
  }

type AdminBlogPostListItem =
  { id :: Int
  , title :: String
  , category :: String
  , created :: ApiDateTime
  , published :: Maybe ApiDateTime
  }

type AdminBlogPost =
  { id :: Int
  , title :: String
  , slug :: String
  , description :: String
  , content :: String
  , tags :: String
  , category :: Int
  , createdAt :: ApiDateTime
  , updatedAt :: ApiDateTime
  , publishedAt :: Maybe ApiDateTime
  , categories :: Array AdminBlogCategory
  }

type AdminBlogCategory =
  { id :: Int
  , title :: String
  }

-- ADMIN MEDIA

type AdminMediaList =
  { basePath :: String
  , contents :: Array AdminMediaListItem
  }

type AdminMediaListItem =
  { name :: String
  , fileType :: FileType
  }

data FileType
  = Audio
  | Video
  | Image
  | Text
  | Directory
  | Other

derive instance eqFileType :: Eq FileType

instance decodeJsonFileType :: DecodeJson FileType where
  decodeJson json = decodeJson json >>= case _ of
    "Audio" -> pure Audio
    "Video" -> pure Video
    "Image" -> pure Image
    "Text" -> pure Text
    "Directory" -> pure Directory
    "Other" -> pure Other
    unknown -> Left <<< TypeMismatch $ "Unknown FileType: " <> unknown
