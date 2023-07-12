module Pages.AdminDashboard (page) where

import Prelude

import Api (class ApiRequest, adminLogout)
import App
  ( class Auth
  , class Navigation
  , class PageDataNotifier
  , isLoggedIn
  , newUrl
  , pageDataReceived
  , setLoggedOut
  )
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Router (AdminRoute(..), Route(..), navLinkAttr)
import Web.UIEvent.MouseEvent as ME

page
  :: forall q i o m
   . Navigation m
  => Auth m
  => ApiRequest m
  => PageDataNotifier m
  => H.Component q i o m
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
  | NavigateTo Route ME.MouseEvent
  | LogOut

handleAction
  :: forall o m
   . Navigation m
  => Auth m
  => ApiRequest m
  => PageDataNotifier m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    isAuthed <- H.lift isLoggedIn
    H.modify_ $ const isAuthed
    H.lift $ pageDataReceived
      { seoData:
          { pageTitle: "Admin Dashboard", metaDescription: "" }
      , apiStatusCode: 200
      }
  NavigateTo route ev -> do
    H.lift $ newUrl route $ Just ev
  LogOut -> H.lift $ do
    setLoggedOut
    void adminLogout
    newUrl Home Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div_
    [ HH.h1_ [ HH.text "Dashboard" ]
    , HH.p_ [ HH.text "Nothing here yet, just a placeholder for testing auth." ]
    , HH.p_ [ HH.text $ "Logged in? " <> show st ]
    , HH.p_
        [ HH.a (navLinkAttr NavigateTo $ Admin AdminBlogPostList)
            [ HH.text "Blog Posts" ]
        ]
    , HH.button [ HP.type_ HP.ButtonButton, HE.onClick $ const LogOut ]
        [ HH.text "Log Out" ]
    ]
