{- | View the list of blog posts.
-}
module Pages.BlogPostList (page) where

import Prelude

import Api (class ApiRequest, blogPostListRequest)
import Api.Types (BlogPostList, BlogPostListItem)
import App (class Navigation, newUrl)
import Data.Array (intersperse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Router (Route(..), navLinkAttr)
import Views.Blog (renderBlogSidebar, renderPostMeta, renderTagList)
import Web.UIEvent.MouseEvent as ME

page :: forall q i o m. ApiRequest m => Navigation m => H.Component q i o m
page =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize }
    }

type State =
  { apiData :: Maybe (Either String BlogPostList)
  }

initialState :: forall i. i -> State
initialState _ = { apiData: Nothing }

data Action
  = Initialize
  | ViewPost String ME.MouseEvent

handleAction
  :: forall o m
   . ApiRequest m
  => Navigation m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    response <- H.lift blogPostListRequest
    H.modify_ _ { apiData = Just response }
  ViewPost slug event ->
    H.lift $ newUrl (ViewBlogPost slug) $ Just event

render :: forall m. State -> H.ComponentHTML Action () m
render = _.apiData >>> case _ of
  Nothing ->
    HH.div_ [ HH.text "Loading..." ]
  Just (Left e) ->
    HH.div_ [ HH.text $ "Error making request: " <> e ]
  Just (Right resp) ->
    HH.div [ HP.classes [ H.ClassName "blog-page" ] ]
      [ HH.div [ HP.classes [ H.ClassName "post-list" ] ]
          $ intersperse (HH.hr [ HP.classes [ H.ClassName "post-separator" ] ])
          $ map renderBlogPost resp.posts
      , renderBlogSidebar ViewPost resp.sidebar
      ]
  where
  renderBlogPost :: forall w. BlogPostListItem -> HH.HTML w Action
  renderBlogPost bpld =
    HH.div_
      [ HH.h2 [ HP.classes [ H.ClassName "post-title" ] ]
          [ postLink bpld bpld.title ]
      , renderPostMeta bpld
      , HH.p [ HP.classes [ H.ClassName "post-description" ] ]
          [ HH.text bpld.description ]
      , renderTagList bpld.tags
      , HH.small_ [ postLink bpld "Read More" ]
      ]

  postLink :: forall r w. { slug :: String | r } -> String -> HH.HTML w Action
  postLink { slug } text =
    HH.a (navLinkAttr (ViewBlogPost slug) $ ViewPost slug) [ HH.text text ]
