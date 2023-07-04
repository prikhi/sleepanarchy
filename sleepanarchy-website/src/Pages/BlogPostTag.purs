{- | View the posts for a tag.
-}
module Pages.BlogPostTag (page, Input) where

import Prelude

import Api (class ApiRequest, ApiError, blogPostTagRequest)
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
      tag = unslugify st.slug
    in
      { pageTitle: "Tag: " <> tag
      , metaDescription: "Blog posts tagged with " <> tag
      }

type Input = String

type State =
  { slug :: String
  , apiData :: RemoteData ApiError BlogPostList
  }

initialState :: Input -> State
initialState slug = { slug, apiData: NotAsked }

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
    response <- H.lift $ blogPostTagRequest slug
    let
      newSlug =
        fromMaybe slug $
          toMaybe response >>= _.posts >>> Array.concatMap _.tags >>> Array.find
            \tag ->
              slugify tag == slugify slug
    H.modify_ _ { apiData = response, slug = newSlug }
  Navigate route event ->
    H.lift $ newUrl route $ Just event
  where
  slugify :: String -> String
  slugify = String.replaceAll (String.Pattern " ") (String.Replacement "-") >>>
    String.toLower

render :: forall m. State -> H.ComponentHTML Action () m
render { apiData, slug } = renderRemoteData apiData $ \resp ->
  let
    headerText = Just $ "Tag: " <> unslugify slug
  in
    HH.div [ HP.classes [ H.ClassName "blog-page" ] ]
      [ renderBlogPostList Navigate resp headerText
      , renderBlogSidebar Navigate resp.sidebar
      ]
