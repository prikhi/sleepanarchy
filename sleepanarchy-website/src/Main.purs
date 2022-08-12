module Main where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web (printError)
import Affjax.Web as AXW
import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson, printJsonDecodeError)
import Data.Bifunctor (bimap, lmap)
import Data.DateTime (DateTime)
import Data.DateTime.Parsing as DTP
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Parsing (parseErrorMessage)

main :: Effect Unit
main = HA.runHalogenAff do
  liftEffect $ log "Starting Up..."
  body <- HA.awaitBody
  runUI root unit body

root :: forall q i o m. MonadAff m => H.Component q i o m
root = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just MakeRequest, handleAction = handleAction }
  }

type State =
  { loading :: Boolean
  , apiData :: Either String BlogPostList
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

data Action = MakeRequest

initialState :: forall i. i -> State
initialState _ = { loading: false, apiData: Left "" }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div [ HP.id "root" ]
    [ HH.div [ HP.class_ $ HH.ClassName "header" ]
        [ HH.div [ HP.class_ $ HH.ClassName "site-logo" ] [ HH.text "Sleep Anarchy" ]
        , HH.div [ HP.class_ $ HH.ClassName "nav" ]
            [ HH.ul_
                [ HH.li_ [ HH.text "Home" ]
                , HH.li_ [ HH.text "Links" ]
                ]
            ]
        ]
    , HH.div [ HP.id "main" ]
        [ renderBlogList st
        ]
    ]

renderBlogList :: forall a m. State -> H.ComponentHTML a () m
renderBlogList st =
  if st.loading then
    HH.div_ [ HH.text "Loading..." ]
  else
    case st.apiData of
      Left e ->
        HH.div_ [ HH.text $ "Error making request: " <> e ]
      Right resp ->
        HH.div_ $ map renderBlogPost resp.posts
  where
  renderBlogPost :: BlogPostListData -> _
  renderBlogPost bpld =
    HH.div_
      [ HH.h2_ [ HH.text bpld.title ]
      , HH.p_ [ HH.text bpld.description ]
      ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  MakeRequest -> do
    H.modify_ _ { loading = true }
    response <- H.liftAff $ AXW.get AXRF.json ("http://localhost:9001/blog/posts")
    H.modify_ _
      { loading = false
      , apiData = (\r -> lmap printJsonDecodeError $ decodeJson $ r.body) =<< lmap printError
          response
      }

data ApiDateTime = ApiDateTime DateTime

instance decodeDateTime :: DecodeJson ApiDateTime where
  decodeJson json = do
    str <- decodeJson json
    bimap (TypeMismatch <<< parseErrorMessage) (\(DTP.FullDateTime dt _) -> ApiDateTime dt) $
      DTP.fromString str
