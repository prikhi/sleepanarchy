{- | View a single blog post.
-}
module Pages.BlogPostView (page) where

import Prelude

import Api (class ApiRequest, ApiError, blogPostDetailsRequest, renderApiError)
import Api.Types (BlogPostDetails)
import App (class Markdown, class Navigation, newUrl, renderMarkdown, renderMarkdownUnsafe)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Html.Renderer.Halogen as RH
import Router (Route)
import Views.Blog (renderBlogSidebar, renderPostMeta, renderTagList)
import Web.UIEvent.MouseEvent as ME

page
  :: forall q o m
   . ApiRequest m
  => Navigation m
  => Markdown m
  => H.Component q String o m
page =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize }
    }

type State =
  { slug :: String
  , apiData :: Maybe (Either ApiError BlogPostDetails)
  , renderedContent :: Maybe String
  }

initialState :: String -> State
initialState slug = { slug, apiData: Nothing, renderedContent: Nothing }

data Action
  = Initialize
  | Navigate Route ME.MouseEvent

handleAction
  :: forall o m
   . ApiRequest m
  => Navigation m
  => Markdown m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    slug <- H.gets _.slug
    response <- H.lift $ blogPostDetailsRequest slug
    htmlContent <- H.lift $ for (hush response) $ renderMarkdown <<< _.content
    H.modify_ _ { apiData = Just response, renderedContent = htmlContent }
  Navigate route event ->
    H.lift $ newUrl route $ Just event

render :: forall w. State -> HH.HTML w Action
render st = case st.apiData of
  Nothing ->
    HH.div_ [ HH.text "Loading..." ]
  Just (Left e) ->
    HH.div_ [ HH.text $ "Error making request. " <> renderApiError e ]
  Just (Right resp) ->
    HH.div [ HP.classes [ H.ClassName "blog-page" ] ]
      [ HH.div [ HP.classes [ H.ClassName "post-details" ] ]
          [ HH.h1 [ HP.classes [ H.ClassName "post-title" ] ]
              [ HH.text resp.title ]
          , renderPostMeta resp
          , HH.div [ HP.classes [ H.ClassName "post-content" ] ]
              [ RH.render_ $ fromMaybe (renderMarkdownUnsafe resp.content)
                  st.renderedContent
              ]
          , renderTagList Navigate resp.tags
          ]
      , renderBlogSidebar Navigate resp.sidebar
      ]
