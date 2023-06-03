{- | View a single blog post.
-}
module Pages.BlogPostView (page) where

import Prelude

import Api (class ApiRequest, ApiError, blogPostDetailsRequest)
import Api.Types (BlogPostDetails)
import App
  ( class Markdown
  , class Navigation
  , class PageDataNotifier
  , mkPageDataNotifierEval
  , newUrl
  , renderMarkdown
  , renderMarkdownUnsafe
  )
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Html.Renderer.Halogen as RH
import Network.RemoteData (RemoteData(..), toMaybe)
import Router (Route)
import Utils (renderRemoteData)
import Views.Blog (renderBlogSidebar, renderPostMeta, renderTagList)
import Web.UIEvent.MouseEvent as ME

page
  :: forall q o m
   . ApiRequest m
  => Navigation m
  => PageDataNotifier m
  => Markdown m
  => H.Component q String o m
page =
  H.mkComponent
    { initialState
    , render
    , eval: mkPageDataNotifierEval H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize }
    }

type State =
  { slug :: String
  , apiData :: RemoteData ApiError BlogPostDetails
  , renderedContent :: Maybe String
  }

initialState :: String -> State
initialState slug = { slug, apiData: NotAsked, renderedContent: Nothing }

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
    H.modify_ _ { apiData = Loading }
    response <- H.lift $ blogPostDetailsRequest slug
    htmlContent <- H.lift $ for (toMaybe response) $ renderMarkdown <<<
      _.content
    H.modify_ _ { apiData = response, renderedContent = htmlContent }
  Navigate route event ->
    H.lift $ newUrl route $ Just event

render :: forall w. State -> HH.HTML w Action
render st = renderRemoteData st.apiData $ \resp ->
  HH.div [ HP.classes [ H.ClassName "blog-page" ] ]
    [ HH.div [ HP.classes [ H.ClassName "post-details" ] ]
        [ HH.h1 [ HP.classes [ H.ClassName "post-title" ] ]
            [ HH.text resp.title ]
        , renderPostMeta Navigate resp
        , HH.div [ HP.classes [ H.ClassName "post-content" ] ]
            [ RH.render_ $ fromMaybe (renderMarkdownUnsafe resp.content)
                st.renderedContent
            ]
        , renderTagList Navigate resp.tags
        ]
    , renderBlogSidebar Navigate resp.sidebar
    ]
