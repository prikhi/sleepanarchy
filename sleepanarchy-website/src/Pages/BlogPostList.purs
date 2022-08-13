module Pages.BlogPostList (page) where

import Prelude

import Api
  ( class ApiRequest
  , BlogPostList
  , BlogPostListItem
  , blogPostListRequest
  )
import App (class Navigation, newUrl)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Router (Route(..))
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
    HH.div_ $ map renderBlogPost resp.posts
  where
  renderBlogPost :: forall w. BlogPostListItem -> HH.HTML w Action
  renderBlogPost bpld =
    HH.div_
      [ HH.h2 [ HE.onClick $ ViewPost bpld.slug ] [ HH.text bpld.title ]
      , HH.p_ [ HH.text bpld.description ]
      ]
