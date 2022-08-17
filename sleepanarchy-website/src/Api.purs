module Api where

import Prelude

import Affjax (Error(..))
import Affjax.RequestHeader as AXRH
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AXW
import Api.Types (BlogPostDetails, BlogPostList)
import Control.Monad.Except (ExceptT(..), except, runExceptT, throwError)
import Data.Argonaut
  ( class DecodeJson
  , JsonDecodeError
  , decodeJson
  , jsonParser
  , printJsonDecodeError
  )
import Data.Bifunctor (lmap)
import Data.Date (Month, Year)
import Data.Either (Either)
import Data.Enum (fromEnum)
import Data.MediaType.Common (applicationJSON)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (ForeignError(..), unsafeToForeign)

-- ENDPOINTS

-- | Possible API endpoints we may hit.
data Endpoint
  = BlogPostListRequest
  | BlogPostArchiveRequest Year Month
  | BlogPostTagRequest String
  | BlogPostCategoryRequest String
  | BlogPostDetailsRequest String

-- | Convert an API endpoint into it's URL, assuming a base path of `/api/`.
endpointUrl :: Endpoint -> String
endpointUrl = (<>) "/api" <<< case _ of
  BlogPostListRequest ->
    "/blog/posts"
  BlogPostArchiveRequest y m ->
    "/blog/posts/archive/" <> show (fromEnum y) <> "/" <> show (fromEnum m)
  BlogPostTagRequest slug ->
    "/blog/posts/tag/" <> slug
  BlogPostCategoryRequest slug ->
    "/blog/posts/category/" <> slug
  BlogPostDetailsRequest slug ->
    "/blog/post/" <> slug

-- ERRORS

data ApiError
  = HttpError AXW.Error
  | StatusCodeError (AXW.Response String)
  | JsonError JsonDecodeError

renderApiError :: ApiError -> String
renderApiError = case _ of
  HttpError e -> AXW.printError e
  JsonError e -> printJsonDecodeError e
  StatusCodeError r -> "There was a problem with the response status: "
    <> show r.status
    <> " - "
    <> r.body

-- EFFECT MONAD

-- | API requests the app can make.
class Monad m <= ApiRequest m where
  blogPostListRequest :: m (Either ApiError BlogPostList)
  blogPostArchiveRequest :: Year -> Month -> m (Either ApiError BlogPostList)
  blogPostTagRequest :: String -> m (Either ApiError BlogPostList)
  blogPostCategoryRequest :: String -> m (Either ApiError BlogPostList)
  blogPostDetailsRequest :: String -> m (Either ApiError BlogPostDetails)

instance appApiRequest :: MonadAff m => ApiRequest m where
  blogPostListRequest = getRequest BlogPostListRequest
  blogPostArchiveRequest y m = getRequest $ BlogPostArchiveRequest y m
  blogPostTagRequest = getRequest <<< BlogPostTagRequest
  blogPostCategoryRequest = getRequest <<< BlogPostCategoryRequest
  blogPostDetailsRequest = getRequest <<< BlogPostDetailsRequest

-- REQUEST HELPERS

-- | Perform a GET request & decode the response as JSON.
-- |
-- | Note that we use a response format of string because errorful status code
-- | response bodies may not decode to JSON but we want to throw a
-- | StatusCodeError in those cases instead of a ResponseBodyError.
getRequest
  :: forall m a. MonadAff m => DecodeJson a => Endpoint -> m (Either ApiError a)
getRequest endpoint = runExceptT $ do
  response <- ExceptT $ lmap HttpError <$> liftAff
    ( AXW.request AXW.defaultRequest
        { responseFormat = AXRF.string
        , headers = [ AXRH.Accept applicationJSON ]
        , url = endpointUrl endpoint
        }
    )
  let (StatusCode responseCode) = response.status
  if responseCode >= 400 then
    throwError $ StatusCodeError response
  else
    except $ decodeResponse response

-- | Decode a JSON response & lift the error to 'ApiError'.
-- |
-- | Returns (HttpError ResponseBodyError) if parsing of the response body to
-- | JSON fails.
-- |
-- | Returns JsonError if parsing of JSON to a given type fails.
decodeResponse
  :: forall a
   . DecodeJson a
  => AXW.Response String
  -> Either ApiError a
decodeResponse response@{ body } = do
  json <- lmap jsonParsingError $ jsonParser body
  lmap JsonError $ decodeJson json
  where
  jsonParsingError :: String -> ApiError
  jsonParsingError e =
    HttpError $
      ResponseBodyError (ForeignError e)
        (response { body = unsafeToForeign body })
