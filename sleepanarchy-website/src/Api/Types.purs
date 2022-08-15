module Api.Types where

import Prelude

import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime, Month, Year)
import Data.DateTime.Parsing as DTP
import Data.Either (Either, note)
import Data.Enum (class BoundedEnum, toEnum)
import Parsing (parseErrorMessage)

-- GENERAL

-- | A 'DateTime' wrapper representing UTC times from the API, with the
-- appropiate decoding instance.
data ApiDateTime = ApiDateTime DateTime

derive instance eqApiDateTime :: Eq ApiDateTime

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
  }

type BlogRecentPost =
  { title :: String, slug :: String }

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
  }

type BlogPostDetails =
  { title :: String
  , content :: String
  , createdAt :: ApiDateTime
  , updatedAt :: ApiDateTime
  , publishedAt :: ApiDateTime
  , sidebar :: BlogSidebar
  }
