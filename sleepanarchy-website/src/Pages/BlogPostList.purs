module Pages.BlogPostList (page) where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AXW
import Api.Types (ApiDateTime)
import Data.Argonaut (decodeJson, printJsonDecodeError)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

page :: forall q i o m. MonadAff m => H.Component q i o m
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

type BlogPostList =
  { posts :: Array BlogPostListData }

type BlogPostListData =
  { title :: String
  , description :: String
  , slug :: String
  , createdAt :: ApiDateTime
  , updatedAt :: ApiDateTime
  , publishedAt :: ApiDateTime
  }

initialState :: forall i. i -> State
initialState _ = { apiData: Nothing }

data Action = Initialize

handleAction
  :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    response <- H.liftAff $ AXW.get AXRF.json "/api/blog/posts"
    H.modify_ _
      { apiData = Just $ join $
          bimap AXW.printError
            (lmap printJsonDecodeError <<< decodeJson <<< _.body)
            response
      }

render :: forall a m. State -> H.ComponentHTML a () m
render = _.apiData >>> case _ of
  Nothing ->
    HH.div_ [ HH.text "Loading..." ]
  Just (Left e) ->
    HH.div_ [ HH.text $ "Error making request: " <> e ]
  Just (Right resp) ->
    HH.div_ $ map renderBlogPost resp.posts
  where
  renderBlogPost :: BlogPostListData -> _
  renderBlogPost bpld =
    HH.div_
      [ HH.h2_ [ HH.text bpld.title ]
      , HH.p_ [ HH.text bpld.description ]
      ]
