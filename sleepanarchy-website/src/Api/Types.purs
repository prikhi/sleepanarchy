module Api.Types where

import Prelude

import Data.Argonaut
  ( class DecodeJson
  , JsonDecodeError(..)
  , decodeJson
  )
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime)
import Data.DateTime.Parsing as DTP
import Parsing (parseErrorMessage)

-- GENERAL

-- | A 'DateTime' wrapper representing UTC times from the API, with the
-- appropiate decoding instance.
data ApiDateTime = ApiDateTime DateTime

instance decodeDateTime :: DecodeJson ApiDateTime where
  decodeJson json = do
    str <- decodeJson json
    bimap (TypeMismatch <<< parseErrorMessage)
      (\(DTP.FullDateTime dt _) -> ApiDateTime dt) $
      DTP.fromString str

-- BLOG POST
type BlogPostList =
  { posts :: Array BlogPostListItem }

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
  }
