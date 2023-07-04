{- | View the list of blog posts.
-}
module Pages.BlogPostList (page) where

import Prelude

import Api (class ApiRequest, ApiError, blogPostListRequest)
import Api.Types (BlogPostList)
import App
  ( class Navigation
  , class PageDataNotifier
  , SEOData
  , mkPageDataNotifierEval
  , newUrl
  )
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Router (Route)
import Utils (renderRemoteData)
import Views.Blog (renderBlogPostList, renderBlogSidebar)
import Web.UIEvent.MouseEvent as ME

page
  :: forall q i o m
   . ApiRequest m
  => Navigation m
  => PageDataNotifier m
  => H.Component q i o m
page =
  H.mkComponent
    { initialState
    , render
    , eval: mkPageDataNotifierEval toSEOData H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  toSEOData :: State -> BlogPostList -> SEOData
  toSEOData _ _ =
    { pageTitle: "Blog"
    , metaDescription:
        "Guides, blog posts, & a link collection on Haskell, Programming, System Administration, & Life."
    }

type State =
  { apiData :: RemoteData ApiError BlogPostList
  }

initialState :: forall i. i -> State
initialState _ = { apiData: NotAsked }

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
    H.modify_ _ { apiData = Loading }
    response <- H.lift blogPostListRequest
    H.modify_ _ { apiData = response }
  Navigate route event ->
    H.lift $ newUrl route $ Just event

render :: forall m. State -> H.ComponentHTML Action () m
render st = renderRemoteData st.apiData $ \resp ->
  HH.div [ HP.classes [ H.ClassName "blog-page" ] ]
    [ renderBlogPostList Navigate resp Nothing
    , renderBlogSidebar Navigate resp.sidebar
    ]
