{- | View a single blog post.
-}
module Pages.BlogPostView (page) where

import Prelude

import Api (class ApiRequest, blogPostDetailsRequest)
import Api.Types (BlogPostDetails)
import App (class Navigation, newUrl)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Router (Route)
import Views.Blog (renderBlogSidebar, renderPostMeta, renderTagList)
import Web.UIEvent.MouseEvent as ME

page :: forall q o m. ApiRequest m => Navigation m => H.Component q String o m
page =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize }
    }

type State =
  { slug :: String
  , apiData :: Maybe (Either String BlogPostDetails)
  }

initialState :: String -> State
initialState slug = { slug, apiData: Nothing }

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
    response <- H.lift $ blogPostDetailsRequest slug
    H.modify_ _ { apiData = Just response }
  Navigate route event ->
    H.lift $ newUrl route $ Just event

render :: forall w. State -> HH.HTML w Action
render = _.apiData >>> case _ of
  Nothing ->
    HH.div_ [ HH.text "Loading..." ]
  Just (Left e) ->
    HH.div_ [ HH.text $ "Error making request: " <> e ]
  Just (Right resp) ->
    HH.div [ HP.classes [ H.ClassName "blog-page" ] ]
      [ HH.div [ HP.classes [ H.ClassName "post-details" ] ]
          [ HH.h1 [ HP.classes [ H.ClassName "post-title" ] ]
              [ HH.text resp.title ]
          , renderPostMeta resp
          , HH.div [ HP.classes [ H.ClassName "post-content" ] ]
              [ HH.text resp.content ]
          , renderTagList Navigate resp.tags
          ]
      , renderBlogSidebar Navigate resp.sidebar
      ]
