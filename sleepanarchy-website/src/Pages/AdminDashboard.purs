module Pages.AdminDashboard (page) where

import Prelude

import App (class Auth, class Navigation, isLoggedIn, newUrl, setLoggedOut)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Router (Route(..))

page :: forall q i o m. Navigation m => Auth m => H.Component q i o m
page =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Initialize, handleAction = handleAction }
    }

type State = Boolean

initialState :: forall i. i -> State
initialState _ = false

data Action
  = Initialize
  | LogOut

handleAction
  :: forall o m
   . Navigation m
  => Auth m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    isAuthed <- H.lift isLoggedIn
    H.modify_ $ const isAuthed
  LogOut -> do
    H.lift setLoggedOut
    H.lift $ newUrl Home Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div_
    [ HH.h1_ [ HH.text "Dashboard" ]
    , HH.p_ [ HH.text "Nothing here yet, just a placeholder for testing auth." ]
    , HH.p_ [ HH.text $ "Logged in? " <> show st ]
    , HH.button [ HP.type_ HP.ButtonButton, HE.onClick $ const LogOut ]
        [ HH.text "Log Out" ]
    ]
