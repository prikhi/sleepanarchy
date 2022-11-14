module Api where

import Prelude

import Affjax (Error(..))
import Affjax.RequestBody as AXRB
import Affjax.RequestHeader as AXRH
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AXW
import Api.Types
  ( AdminBlogCategory
  , AdminBlogPost
  , AdminBlogPostList
  , AdminMediaList
  , BlogPostDetails
  , BlogPostList
  )
import Control.Monad.Except (ExceptT(..), except, runExceptT, throwError)
import Data.Argonaut
  ( class DecodeJson
  , Json
  , JsonDecodeError
  , decodeJson
  , encodeJson
  , jsonParser
  , printJsonDecodeError
  )
import Data.Bifunctor (lmap)
import Data.Date (Month, Year)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Foreign (ForeignError(..), unsafeToForeign)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as E

-- ENDPOINTS

-- | Possible API endpoints we may hit.
data Endpoint
  = BlogPostListRequest
  | BlogPostArchiveRequest Year Month
  | BlogPostTagRequest String
  | BlogPostCategoryRequest String
  | BlogPostDetailsRequest String
  | AdminLogin { name :: String, password :: String }
  | AdminLogout
  | AdminBlogPostListRequest
  | AdminBlogPostRequest Int
  | AdminBlogPostUpdateRequest Int Json
  | AdminBlogPostCreateRequest Json
  | AdminBlogCategoryRequest
  | AdminMediaListRequest (Array String)
  | AdminMediaFolderCreateRequest (Array String) String
  | AdminMediaUploadRequest
      { path :: Array String, name :: String, data :: String }

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
  AdminLogin _ ->
    "/login"
  AdminLogout ->
    "/logout"
  AdminBlogPostListRequest ->
    "/admin/blog/posts"
  AdminBlogPostRequest pId ->
    "/admin/blog/post/" <> show pId
  AdminBlogPostUpdateRequest pId _ ->
    "/admin/blog/post/" <> show pId
  AdminBlogPostCreateRequest _ ->
    "/admin/blog/post"
  AdminBlogCategoryRequest ->
    "/admin/blog/categories"
  AdminMediaListRequest folders ->
    "/admin/media/list/" <> String.joinWith "/" folders
  AdminMediaFolderCreateRequest parentFolders newFolder ->
    "/admin/media/folder/" <> String.joinWith "/" parentFolders <> "/" <>
      newFolder
  AdminMediaUploadRequest _ ->
    "/admin/media/upload"

endpointRequestBody :: Endpoint -> Maybe Json
endpointRequestBody = case _ of
  AdminLogin r -> Just $ encodeJson r
  AdminBlogPostUpdateRequest _ form -> Just form
  AdminBlogPostCreateRequest form -> Just form
  AdminMediaUploadRequest r -> Just $ encodeJson r
  _ -> Nothing

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

-- FORM SUBMISSION

newtype SubmitFormEvent = SubmitFormEvent E.Event

onSubmit
  :: forall r i. (SubmitFormEvent -> i) -> HP.IProp (onSubmit :: E.Event | r) i
onSubmit i = HE.onSubmit (i <<< SubmitFormEvent)

fromSubmitFormEvent :: SubmitFormEvent -> E.Event
fromSubmitFormEvent (SubmitFormEvent e) = e

-- EFFECT MONAD

-- | API requests the app can make.
class Monad m <= ApiRequest m where
  -- | Helper to prevent default on form submissions.
  preventFormSubmission :: SubmitFormEvent -> m Unit
  -- REQUESTS
  blogPostListRequest :: m (Either ApiError BlogPostList)
  blogPostArchiveRequest :: Year -> Month -> m (Either ApiError BlogPostList)
  blogPostTagRequest :: String -> m (Either ApiError BlogPostList)
  blogPostCategoryRequest :: String -> m (Either ApiError BlogPostList)
  blogPostDetailsRequest :: String -> m (Either ApiError BlogPostDetails)
  adminLogin :: String -> String -> m (Either ApiError Unit)
  adminLogout :: m (Either ApiError Unit)
  adminBlogPostListRequest :: m (Either ApiError AdminBlogPostList)
  adminBlogPostRequest :: Int -> m (Either ApiError AdminBlogPost)
  adminBlogPostUpdateRequest :: Int -> Json -> m (Either ApiError Unit)
  adminBlogPostCreateRequest :: Json -> m (Either ApiError Int)
  adminBlogCategoriesRequest :: m (Either ApiError (Array AdminBlogCategory))
  adminMediaListRequest :: Array String -> m (Either ApiError AdminMediaList)
  adminMediaFolderCreateRequest
    :: Array String -> String -> m (Either ApiError Unit)
  adminMediaUploadRequest
    :: String -> String -> Array String -> m (Either ApiError Unit)

instance appApiRequest :: MonadAff m => ApiRequest m where
  preventFormSubmission (SubmitFormEvent e) = liftEffect $ E.preventDefault e
  blogPostListRequest = getRequest BlogPostListRequest
  blogPostArchiveRequest y m = getRequest $ BlogPostArchiveRequest y m
  blogPostTagRequest = getRequest <<< BlogPostTagRequest
  blogPostCategoryRequest = getRequest <<< BlogPostCategoryRequest
  blogPostDetailsRequest = getRequest <<< BlogPostDetailsRequest
  adminLogin name password = noContentPostRequest $ AdminLogin
    { name, password }
  adminLogout = noContentPostRequest AdminLogout
  adminBlogPostListRequest = getRequest AdminBlogPostListRequest
  adminBlogPostRequest = getRequest <<< AdminBlogPostRequest
  adminBlogPostUpdateRequest pId form = noContentPostRequest $
    AdminBlogPostUpdateRequest pId form
  adminBlogPostCreateRequest = postRequest <<< AdminBlogPostCreateRequest
  adminBlogCategoriesRequest = getRequest AdminBlogCategoryRequest
  adminMediaListRequest = getRequest <<< AdminMediaListRequest
  adminMediaFolderCreateRequest parents = noContentPostRequest <<<
    AdminMediaFolderCreateRequest parents
  adminMediaUploadRequest name data_ path = noContentPostRequest $
    AdminMediaUploadRequest { name, path, data: data_ }

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

noContentPostRequest
  :: forall m. MonadAff m => Endpoint -> m (Either ApiError Unit)
noContentPostRequest endpoint = runExceptT $ do
  response <- ExceptT $ lmap HttpError <$> liftAff
    ( AXW.request AXW.defaultRequest
        { responseFormat = AXRF.string
        , url = endpointUrl endpoint
        , content = AXRB.json <$> endpointRequestBody endpoint
        , method = Left POST
        }
    )
  let (StatusCode responseCode) = response.status
  if responseCode >= 400 then
    throwError $ StatusCodeError response
  else
    pure unit

postRequest
  :: forall m a. MonadAff m => DecodeJson a => Endpoint -> m (Either ApiError a)
postRequest endpoint = runExceptT $ do
  response <- ExceptT $ lmap HttpError <$> liftAff
    ( AXW.request AXW.defaultRequest
        { responseFormat = AXRF.string
        , url = endpointUrl endpoint
        , content = AXRB.json <$> endpointRequestBody endpoint
        , method = Left POST
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
