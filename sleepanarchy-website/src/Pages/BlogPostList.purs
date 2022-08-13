module Pages.BlogPostList (page) where

import Prelude

import Api
  ( class ApiRequest
  , BlogPostList
  , BlogPostListItem
  , blogPostListRequest
  )
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

page :: forall q i o m. ApiRequest m => H.Component q i o m
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

data Action = Initialize

handleAction
  :: forall o m. ApiRequest m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    response <- H.lift blogPostListRequest
    H.modify_ _ { apiData = Just response }

render :: forall a m. State -> H.ComponentHTML a () m
render = _.apiData >>> case _ of
  Nothing ->
    HH.div_ [ HH.text "Loading..." ]
  Just (Left e) ->
    HH.div_ [ HH.text $ "Error making request: " <> e ]
  Just (Right resp) ->
    HH.div_ $ map renderBlogPost resp.posts
  where
  renderBlogPost :: forall w i. BlogPostListItem -> HH.HTML w i
  renderBlogPost bpld =
    HH.div_
      [ HH.h2_ [ HH.text bpld.title ]
      , HH.p_ [ HH.text bpld.description ]
      ]
