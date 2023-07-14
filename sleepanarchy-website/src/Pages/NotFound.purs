module Pages.NotFound (page) where

import Prelude

import App (class PageDataNotifier, pageDataReceived)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

page
  :: forall q i o m
   . PageDataNotifier m
  => H.Component q i o m
page = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize, handleAction = handleAction }
  }

type State = Unit

initialState :: forall i. i -> State
initialState = const unit

data Action = Initialize

handleAction
  :: forall o m
   . PageDataNotifier m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize ->
    H.lift $ pageDataReceived
      { seoData:
          { pageTitle: "404 - Not Found"
          , metaDescription: ""
          }
      , apiStatusCode: 200
      }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div_
    [ HH.h1_ [ HH.text $ "404 - Page Not Found" ]
    , HH.p_
        [ HH.text
            "Sorry, this URL could not be found on the server. Please use the nav menu to try to find the page that you were looking for."
        ]
    ]
