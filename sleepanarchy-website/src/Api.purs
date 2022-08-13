module Api where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AXW
import Api.Types (ApiDateTime)
import Data.Argonaut (class DecodeJson, Json, decodeJson, printJsonDecodeError)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either)
import Effect.Aff.Class (class MonadAff, liftAff)

-- ENDPOINTS

data Endpoint
  = BlogPostListRequest
  | BlogPostDetailsRequest String

endpointUrl :: Endpoint -> String
endpointUrl = (<>) "/api" <<< case _ of
  BlogPostListRequest ->
    "/blog/posts"
  BlogPostDetailsRequest slug ->
    "/blog/post/" <> slug

-- EFFECT MONAD

class Monad m <= ApiRequest m where
  blogPostListRequest :: m (Either String BlogPostList)
  blogPostDetailsRequest :: String -> m (Either String BlogPostDetails)

instance appApiRequest :: MonadAff m => ApiRequest m where
  blogPostListRequest = bplRequest
  blogPostDetailsRequest = bpdRequest

-- REQUEST HELPERS

getRequest
  :: forall m a. MonadAff m => DecodeJson a => Endpoint -> m (Either String a)
getRequest endpoint =
  decodeResponse <$> liftAff (AXW.get AXRF.json $ endpointUrl endpoint)

decodeResponse
  :: forall a
   . DecodeJson a
  => Either AXW.Error (AXW.Response Json)
  -> Either String a
decodeResponse =
  bimap AXW.printError (_.body >>> decodeJson >>> lmap printJsonDecodeError) >>>
    join

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

bplRequest :: forall m. MonadAff m => m (Either String BlogPostList)
bplRequest = getRequest BlogPostListRequest

type BlogPostDetails =
  { title :: String
  , content :: String
  , createdAt :: ApiDateTime
  , updatedAt :: ApiDateTime
  , publishedAt :: ApiDateTime
  }

bpdRequest
  :: forall m. MonadAff m => String -> m (Either String BlogPostDetails)
bpdRequest = getRequest <<< BlogPostDetailsRequest
