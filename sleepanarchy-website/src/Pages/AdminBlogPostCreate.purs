module Pages.AdminBlogPostCreate (page) where

import Prelude

import Api
  ( class ApiRequest
  , ApiError
  , SubmitFormEvent
  , adminBlogCategoriesRequest
  , adminBlogPostCreateRequest
  , onSubmit
  , preventFormSubmission
  , renderApiError
  )
import Api.Types (AdminBlogCategory)
import App (class Navigation, newUrl)
import Data.Argonaut (encodeJson)
import Data.Array as Array
import Data.Either (Either(..), fromRight)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Router (AdminRoute(..), Route(..))
import Views.Forms (mkCheckbox, mkInput, mkSelect, mkSubmit, mkTextArea)

page :: forall q i o m. ApiRequest m => Navigation m => H.Component q i o m
page = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

type State =
  { formData :: FormData
  , submitResponse :: Maybe ApiError
  , categoriesResponse :: Maybe (Either ApiError (Array AdminBlogCategory))
  }

type FormData =
  { title :: String
  , slug :: Maybe String
  , content :: String
  , description :: String
  , tags :: String
  , publish :: Boolean
  , categoryId :: Int
  }

initialState :: forall i. i -> State
initialState _ =
  { formData:
      { title: ""
      , slug: Nothing
      , content: ""
      , description: ""
      , tags: ""
      , publish: false
      , categoryId: 0
      }
  , submitResponse: Nothing
  , categoriesResponse: Nothing
  }

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
  :: forall o m
   . ApiRequest m
  => Navigation m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    response <- H.lift $ adminBlogCategoriesRequest
    H.modify_ _ { categoriesResponse = Just response }
  SetTitle str ->
    H.modify_ \st -> st { formData = st.formData { title = str } }
  SetSlug str ->
    H.modify_ \st -> st
      { formData = st.formData
          { slug = if String.null str then Nothing else Just str }
      }
  SetContent str ->
    H.modify_ \st -> st { formData = st.formData { content = str } }
  SetDescription str ->
    H.modify_ \st -> st { formData = st.formData { description = str } }
  SetTags str ->
    H.modify_ \st -> st { formData = st.formData { tags = str } }
  SetPublish val ->
    H.modify_ \st -> st { formData = st.formData { publish = val } }
  SetCategoryId str ->
    case Int.fromString str of
      Nothing -> pure unit
      Just id ->
        H.modify_ \st -> st { formData = st.formData { categoryId = id } }
  MakeRequest ev -> do
    H.lift $ preventFormSubmission ev
    st <- H.get
    H.lift (adminBlogPostCreateRequest (encodeJson st.formData)) >>=
      case _ of
        Right pId ->
          H.lift $ newUrl (Admin $ AdminBlogPostEdit pId) Nothing
        Left err ->
          H.modify_ _ { submitResponse = Just err }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  let
    mkInput_ l h s = mkInput st.formData unit l h s (const Nothing)
    mkTextArea_ l h s = mkTextArea st.formData unit l h s (const Nothing)
    mkCheckbox_ l h s = mkCheckbox st.formData unit l h s (const Nothing)
    mkSelect_ l h s = mkSelect st.formData unit l h s (const Nothing)
  in
    HH.div [ HP.classes [ H.ClassName "admin-post-create" ] ]
      [ HH.h1_ [ HH.text $ "Create New Blog Post" ]
      , HH.form [ onSubmit MakeRequest ]
          [ mkInput_ "Title" Nothing _.title SetTitle
          , mkInput_ "Slug" (Just "Leave blank to auto-generate")
              (fromMaybe "" <<< _.slug)
              SetSlug
          , mkTextArea_ "Content" Nothing _.content SetContent
          , mkTextArea_ "Description" (Just "Leave blank to auto-generate")
              _.description
              SetDescription
          , mkInput_ "Tags" (Just "Comma-separated list") _.tags SetTags
          , mkSelect_ "Category" Nothing _.categoryId
              ( Array.cons { id: 0, title: "Select a Category" } $ maybe []
                  (fromRight [])
                  st.categoriesResponse
              )
              (\{ id, title } -> { id, text: title })
              SetCategoryId
          , mkCheckbox_ "Publish" Nothing _.publish SetPublish
          , mkSubmit "Create"
          , errMsg
          ]
      ]
  where
  errMsg :: forall w a. HH.HTML w a
  errMsg =
    case st.submitResponse of
      Nothing -> HH.text ""
      Just err ->
        HH.p_
          [ HH.text $ "An error occured when saving: " <> renderApiError err ]
