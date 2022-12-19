module BaseAdmin (component) where

import Prelude

import Api (class ApiRequest, adminLogout)
import App
  ( class Auth
  , class FileUpload
  , class Navigation
  , newUrl
  , setLoggedOut
  )
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Pages.AdminBlogPostCreate as AdminBlogPostCreate
import Pages.AdminBlogPostEdit as AdminBlogPostEdit
import Pages.AdminBlogPostList as AdminBlogPostList
import Pages.AdminDashboard as AdminDashboard
import Pages.AdminLogin as AdminLogin
import Pages.AdminMediaList as AdminMediaList
import Router (AdminRoute(..), Route(..), navLinkAttr)
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent as ME

component
  :: forall q o m
   . ApiRequest m
  => Auth m
  => FileUpload m
  => Navigation m
  => H.Component q AdminRoute o m
component = H.mkComponent
  { initialState: initial
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }

type Slots =
  ( viewAdminLoginSlot :: forall query. H.Slot query Void (Maybe String)
  , viewAdminDashboardSlot :: forall query. H.Slot query Void Unit
  , viewAdminBlogPostListSlot :: forall query. H.Slot query Void Unit
  , viewAdminBlogPostEditSlot :: forall query. H.Slot query Void Int
  , viewAdminBlogPostCreateSlot :: forall query. H.Slot query Void Unit
  , viewAdminMediaListSlot :: forall query. H.Slot query Void (Array String)
  )

_viewAdminLogin :: Proxy "viewAdminLoginSlot"
_viewAdminLogin = Proxy

_viewAdminDashboard :: Proxy "viewAdminDashboardSlot"
_viewAdminDashboard = Proxy

_viewAdminBlogPostList :: Proxy "viewAdminBlogPostListSlot"
_viewAdminBlogPostList = Proxy

_viewAdminBlogPostEdit :: Proxy "viewAdminBlogPostEditSlot"
_viewAdminBlogPostEdit = Proxy

_viewAdminBlogPostCreate :: Proxy "viewAdminBlogPostCreateSlot"
_viewAdminBlogPostCreate = Proxy

_viewAdminMediaList :: Proxy "viewAdminMediaListSlot"
_viewAdminMediaList = Proxy

type State = { currentPage :: AdminRoute }

initial :: AdminRoute -> State
initial currentPage = { currentPage }

data Action
  = NavClick Route ME.MouseEvent
  | LogOut ME.MouseEvent

handleAction
  :: forall o m
   . Auth m
  => ApiRequest m
  => Navigation m
  => Action
  -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  NavClick route event ->
    H.lift $ newUrl route $ Just event
  LogOut event -> H.lift $ do
    setLoggedOut
    void adminLogout
    newUrl Home (Just event)

render
  :: forall m
   . ApiRequest m
  => Navigation m
  => FileUpload m
  => Auth m
  => State
  -> H.ComponentHTML Action Slots m
render { currentPage } =
  case currentPage of
    Login _ ->
      renderPage currentPage
    _ ->
      HH.div [ HP.classes [ H.ClassName "admin-site" ] ]
        [ renderSidebar currentPage
        , HH.div [ HP.classes [ H.ClassName "admin-content" ] ]
            [ renderPage currentPage ]

        ]

-- TODO: Add icons to sidebar, make collapseable.
renderSidebar :: forall s m. AdminRoute -> H.ComponentHTML Action s m
renderSidebar currentPage =
  let
    activePageClass route =
      if isInPageHierarchy route then [ H.ClassName "active" ] else []
    navLink text route =
      HH.a
        ((navLinkAttr NavClick route) <> [ HP.classes (activePageClass route) ])
        [ HH.text text ]
  in
    HH.div [ HP.classes [ H.ClassName "admin-sidebar" ] ]
      [ HH.h4_ [ HH.text "Sleep Anarchy Admin" ]
      , HH.ul
          [ HP.classes [ H.ClassName "nav-links" ] ]
          [ HH.li_ [ navLink "Dashboard" (Admin Dashboard) ]
          , HH.li_ [ navLink "Blog Posts" (Admin AdminBlogPostList) ]
          , HH.li_ [ navLink "Media" (Admin (AdminMediaList [])) ]
          , HH.li_ [ navLink "Site Home" Home ]
          ]
      , HH.p_ [ HH.a (navLinkAttr (const LogOut) Home) [ HH.text "Logout" ] ]
      ]
  where
  isInPageHierarchy :: Route -> Boolean
  isInPageHierarchy navPage = navPage == Admin currentPage ||
    case navPage of
      Admin adminPage -> case Tuple adminPage currentPage of
        Tuple AdminBlogPostList (AdminBlogPostEdit _) -> true
        Tuple AdminBlogPostList (AdminBlogPostCreate) -> true
        Tuple (AdminMediaList _) (AdminMediaList _) -> true
        _ -> false
      _ -> false

renderPage
  :: forall a m
   . ApiRequest m
  => Navigation m
  => FileUpload m
  => Auth m
  => AdminRoute
  -> H.ComponentHTML a Slots m
renderPage = case _ of
  Login mbRedirect ->
    HH.slot_ _viewAdminLogin mbRedirect AdminLogin.page mbRedirect
  Dashboard ->
    HH.slot_ _viewAdminDashboard unit AdminDashboard.page unit
  AdminBlogPostList ->
    HH.slot_ _viewAdminBlogPostList unit AdminBlogPostList.page unit
  AdminBlogPostEdit postId ->
    HH.slot_ _viewAdminBlogPostEdit postId AdminBlogPostEdit.page postId
  AdminBlogPostCreate ->
    HH.slot_ _viewAdminBlogPostCreate unit AdminBlogPostCreate.page unit
  AdminMediaList folders ->
    HH.slot_ _viewAdminMediaList folders AdminMediaList.page folders
