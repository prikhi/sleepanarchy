{- | View the posts for a category.
-}
module Pages.BlogPostCategory (page, Input) where

import Prelude

import Api (class ApiRequest, ApiError, blogPostCategoryRequest)
import Api.Types (BlogPostList)
import App
  ( class Navigation
  , class PageDataNotifier
  , SEOData
  , mkPageDataNotifierEval
  , newUrl
  )
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), toMaybe)
import Router (Route)
import Utils (renderRemoteData, unslugify)
import Views.Blog (renderBlogPostList, renderBlogSidebar)
import Web.UIEvent.MouseEvent as ME

page
  :: forall q o m
   . ApiRequest m
  => PageDataNotifier m
  => Navigation m
  => H.Component q Input o m
page = H.mkComponent
  { initialState
  , render
  , eval: mkPageDataNotifierEval toSEOData H.defaultEval
      { handleAction = handleAction, initialize = Just Initialize }
  }
  where
  toSEOData :: State -> BlogPostList -> SEOData
  toSEOData st _ =
    let
      category = fromMaybe (unslugify st.slug) st.title
    in
      { pageTitle: "Category: " <> category
      , metaDescription: "Blog posts in the " <> category <> " category."
      }

type Input = String

type State =
  { slug :: String
  , title :: Maybe String
  , apiData :: RemoteData ApiError BlogPostList
  }

initialState :: Input -> State
initialState slug = { slug, apiData: NotAsked, title: Nothing }

data Action
  = Initialize
  | Navigate Route ME.MouseEvent

handleAction
  :: forall o m
   . ApiRequest m
  => Navigation m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    slug <- H.gets _.slug
    H.modify_ _ { apiData = Loading }
    response <- H.lift $ blogPostCategoryRequest $ slugify slug
    let
      title =
        toMaybe response >>= _.posts >>> map (_.category >>> _.title) >>>
          Array.head
    H.modify_ _ { apiData = response, title = title }
  Navigate route event ->
    H.lift $ newUrl route $ Just event
  where
  slugify :: String -> String
  slugify = String.replaceAll (String.Pattern " ") (String.Replacement "-") >>>
    String.toLower

render :: forall m. State -> H.ComponentHTML Action () m
render { apiData, slug, title } = renderRemoteData apiData $ \resp ->
  let
    headerText = Just $ "Category: " <> fromMaybe (unslugify slug) title
  in
    HH.div [ HP.classes [ H.ClassName "blog-page" ] ]
      [ renderBlogPostList Navigate resp headerText
      , renderBlogSidebar Navigate resp.sidebar
      ]
