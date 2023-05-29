module Pages.AdminBlogPostEdit (page, encodeFormData) where

import Prelude

import Api
  ( class ApiRequest
  , ApiError
  , SubmitFormEvent
  , adminBlogPostRequest
  , adminBlogPostUpdateRequest
  , onSubmit
  , preventFormSubmission
  , renderApiError
  )
import Api.Types (AdminBlogPost)
import Data.Argonaut (Json, encodeJson, (:=?), (~>?))
import Data.Array (foldr)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Utils (renderRemoteData, showDate)
import Views.Forms (mkCheckbox, mkInput, mkSelect, mkSubmit, mkTextArea)

page :: forall q o m. ApiRequest m => H.Component q Int o m
page = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction, initialize = Just Initialize }
  }

type State =
  { postId :: Int
  , formData :: FormData
  , apiData :: RemoteData ApiError AdminBlogPost
  , submitResponse :: RemoteData ApiError Unit
  }

type FormData =
  { title :: Maybe String
  , slug :: Maybe String
  , content :: Maybe String
  , description :: Maybe String
  , tags :: Maybe String
  , publish :: Maybe Boolean
  , categoryId :: Maybe Int
  }

initialState :: Int -> State
initialState postId =
  { postId
  , formData:
      { title: Nothing
      , slug: Nothing
      , content: Nothing
      , description: Nothing
      , tags: Nothing
      , publish: Nothing
      , categoryId: Nothing
      }
  , apiData: NotAsked
  , submitResponse: NotAsked
  }

encodeFormData :: FormData -> Json
encodeFormData f =
  let
    fields =
      [ "title" :=? f.title
      , "slug" :=? f.slug
      , "content" :=? f.content
      , "description" :=? f.description
      , "tags" :=? f.tags
      , "published" :=? f.publish
      , "categoryId" :=? f.categoryId
      ]
  in
    foldr (~>?) (encodeJson {}) fields

data Action
  = Initialize
  | SetTitle String
  | SetSlug String
  | SetContent String
  | SetDescription String
  | SetTags String
  | SetPublish Boolean
  | SetCategoryId String
  | MakeRequest SubmitFormEvent

handleAction
  :: forall o m. ApiRequest m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    postId <- H.gets _.postId
    H.modify_ _ { apiData = Loading }
    response <- H.lift $ adminBlogPostRequest postId
    -- TODO: if 401, call logout & redirect to login page?
    H.modify_ _ { apiData = response }
  SetTitle str ->
    H.modify_ \st -> st { formData = st.formData { title = Just str } }
  SetSlug str ->
    H.modify_ \st -> st { formData = st.formData { slug = Just str } }
  SetContent str ->
    H.modify_ \st -> st { formData = st.formData { content = Just str } }
  SetDescription str ->
    H.modify_ \st -> st { formData = st.formData { description = Just str } }
  SetTags str ->
    H.modify_ \st -> st { formData = st.formData { tags = Just str } }
  SetPublish val ->
    H.modify_ \st -> st { formData = st.formData { publish = Just val } }
  SetCategoryId str ->
    case Int.fromString str of
      Nothing -> pure unit
      Just id ->
        H.modify_ \st -> st { formData = st.formData { categoryId = Just id } }
  MakeRequest ev -> do
    H.lift $ preventFormSubmission ev
    st <- H.get
    H.modify_ _ { submitResponse = Loading }
    response <- H.lift $ adminBlogPostUpdateRequest st.postId
      (encodeFormData st.formData)
    H.modify_ _ { submitResponse = response }

render :: forall m. State -> H.ComponentHTML Action () m
render st = renderRemoteData st.apiData $ \resp ->
  let
    mkInput_ = mkInput resp st.formData
    mkTextArea_ = mkTextArea resp st.formData
  in
    HH.div [ HP.classes [ H.ClassName "admin-post-edit" ] ]
      [ HH.h1_
          [ HH.text $ "Edit Blog Post #" <> show st.postId
          , HH.br_
          , HH.text resp.title
          ]
      , HH.small_
          [ HH.text $ "Created: " <> showDate resp.createdAt
          , HH.text " | "
          , HH.text $ "Updated: " <> showDate resp.updatedAt
          , HH.text " | "
          , HH.text $ "Published: " <> maybe "Never" showDate resp.publishedAt
          ]
      , HH.form [ onSubmit MakeRequest ]
          [ mkInput_ "Title" Nothing _.title _.title SetTitle
          , mkInput_ "Slug" (Just "Leave blank to auto-generate")
              _.slug
              _.slug
              SetSlug
          , mkTextArea_ "Content" Nothing _.content _.content SetContent
          , mkTextArea_ "Description" (Just "Leave blank to auto-generate")
              _.description
              _.description
              SetDescription
          , mkInput_ "Tags" (Just "Comma-separated list")
              _.tags
              _.tags
              SetTags
          , mkSelect resp st.formData "Category" Nothing _.category
              _.categoryId
              resp.categories
              (\{ id, title } -> { id, text: title })
              SetCategoryId
          , mkCheckbox resp st.formData "Publish" Nothing
              (_.publishedAt >>> isJust)
              _.publish
              SetPublish
          , mkSubmit "Update"
          , errMsg
          ]
      ]
  where
  errMsg :: forall w a. HH.HTML w a
  errMsg =
    case st.submitResponse of
      NotAsked -> HH.text ""
      Loading -> HH.text "Updating Post..."
      Success _ -> HH.text "Update Successful"
      Failure err ->
        HH.p_
          [ HH.text $ "An error occured when saving: " <> renderApiError err ]
