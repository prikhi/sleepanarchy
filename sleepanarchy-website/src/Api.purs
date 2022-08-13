module Api where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AXW
import Api.Types (BlogPostDetails, BlogPostList)
import Data.Argonaut (class DecodeJson, Json, decodeJson, printJsonDecodeError)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either)
import Effect.Aff.Class (class MonadAff, liftAff)

-- ENDPOINTS

-- | Possible API endpoints we may hit.
data Endpoint
  = BlogPostListRequest
  | BlogPostDetailsRequest String

-- | Convert an API endpoint into it's URL, assuming a base path of `/api/`.
endpointUrl :: Endpoint -> String
endpointUrl = (<>) "/api" <<< case _ of
  BlogPostListRequest ->
    "/blog/posts"
  BlogPostDetailsRequest slug ->
    "/blog/post/" <> slug

-- EFFECT MONAD

-- | API requests the app can make.
class Monad m <= ApiRequest m where
  blogPostListRequest :: m (Either String BlogPostList)
  blogPostDetailsRequest :: String -> m (Either String BlogPostDetails)

instance appApiRequest :: MonadAff m => ApiRequest m where
  blogPostListRequest = getRequest BlogPostListRequest
  blogPostDetailsRequest = getRequest <<< BlogPostDetailsRequest

-- REQUEST HELPERS

-- | Perform a GET request & decode the response as JSON.
getRequest
  :: forall m a. MonadAff m => DecodeJson a => Endpoint -> m (Either String a)
getRequest endpoint =
  decodeResponse <$> liftAff (AXW.get AXRF.json $ endpointUrl endpoint)

-- | Decode a JSON response & unify the HTTP & JSON errors as Strings.
decodeResponse
  :: forall a
   . DecodeJson a
  => Either AXW.Error (AXW.Response Json)
  -> Either String a
decodeResponse =
  bimap AXW.printError (_.body >>> decodeJson >>> lmap printJsonDecodeError) >>>
    join
