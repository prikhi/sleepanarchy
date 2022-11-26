module BaseSite (Query(..), component) where

import Prelude

import Api (class ApiRequest)
import App
  ( class Auth
  , class FileUpload
  , class GetTime
  , class Markdown
  , class Navigation
  , getToday
  , isLoggedIn
  , newUrl
  )
import Data.Date (Date, canonicalDate, year)
import Data.Enum (fromEnum)
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
import Pages.BlogPostArchive as BlogPostArchive
import Pages.BlogPostCategory as BlogPostCategory
import Pages.BlogPostList as BlogPostList
import Pages.BlogPostTag as BlogPostTag
import Pages.BlogPostView as BlogPostView
import Pages.LinkCategoryView as LinkCategoryView
import Pages.LinkView as LinkView
import Router
  ( AdminRoute(..)
  , Route(..)
  , navLinkAttr
  , parseRedirectPath
  , reverse
  )
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent as ME

-- | The component representing the entire Halogen app. This has slots for each
-- | distinct page type and switches on the stored Route to determine which to
-- | render. It accepts route updates from outside the Halogen environment.
component
  :: forall o m
   . GetTime m
  => ApiRequest m
  => Auth m
  => FileUpload m
  => Navigation m
  => Markdown m
  => H.Component Query Route o m
component = H.mkComponent
  { initialState: initial
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      }
  }

data Query a
  -- | Set the route to the given value.
  = UpdateRoute Route a

data Action
  -- | Initialize the site by fetching the current date.
  = Initialize
  -- | Respond to a nav menu click by changing the route & url.
  | NavClick Route ME.MouseEvent

type Slots =
  ( homePageSlot :: forall query. H.Slot query Void Unit
  , viewBlogPostSlot :: forall query. H.Slot query Void String
  , viewBlogArchiveSlot :: forall query. H.Slot query Void BlogPostArchive.Input
  , viewBlogTagSlot :: forall query. H.Slot query Void BlogPostTag.Input
  , viewBlogCategorySlot ::
      forall query. H.Slot query Void BlogPostCategory.Input
  , viewLinks :: forall query. H.Slot query Void Unit
  , viewLinkCategory :: forall query. H.Slot query Void String
  -- TODO: shall we have a single slot for all Admin routes w/ an BaseAdmin component?
  , viewAdminLoginSlot :: forall query. H.Slot query Void (Maybe String)
  , viewAdminDashboardSlot :: forall query. H.Slot query Void Unit
  , viewAdminBlogPostListSlot :: forall query. H.Slot query Void Unit
  , viewAdminBlogPostEditSlot :: forall query. H.Slot query Void Int
  , viewAdminBlogPostCreateSlot :: forall query. H.Slot query Void Unit
  , viewAdminMediaListSlot :: forall query. H.Slot query Void (Array String)
  )

_homePage :: Proxy "homePageSlot"
_homePage = Proxy

_viewBlogPost :: Proxy "viewBlogPostSlot"
_viewBlogPost = Proxy

_viewBlogArchive :: Proxy "viewBlogArchiveSlot"
_viewBlogArchive = Proxy

_viewBlogPostTag :: Proxy "viewBlogTagSlot"
_viewBlogPostTag = Proxy

_viewBlogPostCategory :: Proxy "viewBlogCategorySlot"
_viewBlogPostCategory = Proxy

_viewLinks :: Proxy "viewLinks"
_viewLinks = Proxy

_viewLinkCategory :: Proxy "viewLinkCategory"
_viewLinkCategory = Proxy

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

-- | The base app only cares about the current page & date, all other state is
-- | stored within the various `Page.*` module componets.
type State = { currentPage :: Route, currentDate :: Date }

initial :: Route -> State
initial route =
  { currentPage: route
  , currentDate: canonicalDate bottom bottom bottom
  }

handleQuery
  :: forall o m a
   . Auth m
  => Navigation m
  => Query a
  -> H.HalogenM State Action Slots o m (Maybe a)
handleQuery = case _ of
  UpdateRoute newRoute next -> do
    isAuthed <- H.lift isLoggedIn
    let setPageToNewRoute = H.modify_ (_ { currentPage = newRoute })
    case newRoute of
      Admin (Login mbRedirectPath) ->
        if isAuthed then
          H.lift $ newUrl (parseRedirectPath mbRedirectPath) Nothing
        else
          setPageToNewRoute
      Admin adminRoute ->
        if not isAuthed then do
          let
            mbRedirectTo =
              if adminRoute == Dashboard then Nothing
              else Just $ reverse newRoute
          H.lift $ newUrl (Admin $ Login mbRedirectTo) Nothing
        else
          setPageToNewRoute
      _ ->
        setPageToNewRoute
    pure $ Just next

handleAction
  :: forall m o
   . GetTime m
  => Navigation m
  => Action
  -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    today <- H.lift getToday
    H.modify_ _ { currentDate = today }
  NavClick route event ->
    H.lift $ newUrl route $ Just event

-- | Render the Header & Page Content.
render
  :: forall m
   . ApiRequest m
  => Navigation m
  => Auth m
  => FileUpload m
  => Markdown m
  => State
  -> H.ComponentHTML Action Slots m
render { currentPage, currentDate } =
  case currentPage of
    Admin adminRoute ->
      renderAdmin adminRoute
    _ ->
      HH.div [ HP.id "root" ]
        [ renderHeader currentPage
        , HH.div [ HP.id "main" ]
            [ renderPage currentPage
            ]
        , renderFooter currentDate
        ]

renderHeader :: forall s m. Route -> H.ComponentHTML Action s m
renderHeader currentPage =
  let
    activePageClass route =
      if isInPageHierarchy route then [ H.ClassName "active" ] else []
    navLink text route =
      HH.a
        ((navLinkAttr NavClick route) <> [ HP.classes (activePageClass route) ])
        [ HH.text text ]
  in
    HH.nav [ HP.class_ $ HH.ClassName "header" ]
      [ HH.div [ HP.class_ $ HH.ClassName "site-logo" ]
          [ navLink "Sleep Anarchy" Home ]
      , HH.div [ HP.class_ $ HH.ClassName "nav" ]
          [ HH.ul_
              [ HH.li_ [ navLink "Home" Home ]
              , HH.li_ [ navLink "Links" ViewLinks ]
              ]
          ]
      ]
  where
  isInPageHierarchy :: Route -> Boolean
  isInPageHierarchy navPage = navPage == currentPage ||
    case Tuple navPage currentPage of
      Tuple Home (ViewBlogPost _) -> true
      Tuple Home (ViewBlogArchive _ _) -> true
      Tuple Home (ViewBlogTag _) -> true
      Tuple Home (ViewBlogCategory _) -> true
      Tuple ViewLinks (ViewLinkCategory _) -> true
      _ -> false

renderFooter :: forall w i. Date -> HH.HTML w i
renderFooter currentDate =
  HH.footer_
    [ HH.div
        [ HP.classes [ H.ClassName "site-info", H.ClassName "text-center" ] ]
        [ HH.small_
            [ HH.p_
                [ HH.text
                    "Except where otherwise noted, content on this site is licensed under a "
                , externalLink
                    "Creative Commons BY-NC-SA 4.0 International License"
                    "https://creativecommons.org/licenses/by-nc-sa/4.0/"
                , HH.text $ ". Copyleft 2014-" <> currentYear <> "."
                ]
            , HH.p_
                [ HH.text
                    "This site is built with "
                , externalLink "Purescript" "https://www.purescript.org"
                , HH.text " & "
                , externalLink "Haskell" "https://www.haskell.org"
                , HH.text ". Source code is available "
                , externalLink "on GitHub"
                    "https://github.com/prikhi/sleepanarchy"
                , HH.text "."
                ]
            ]
        ]

    ]
  where
  externalLink text url =
    HH.a [ HP.href url, HP.target "_blank" ] [ HH.text text ]
  currentYear = show (fromEnum $ year currentDate)

-- | Render the correct slot for each Route.
renderPage
  :: forall a m
   . ApiRequest m
  => Navigation m
  => Markdown m
  => Route
  -> H.ComponentHTML a Slots m
renderPage = pageWrapper <<< case _ of
  Home ->
    HH.slot_ _homePage unit BlogPostList.page unit
  ViewBlogPost slug ->
    HH.slot_ _viewBlogPost slug BlogPostView.page slug
  ViewBlogArchive year month ->
    HH.slot_ _viewBlogArchive (Tuple year month) BlogPostArchive.page
      (Tuple year month)
  ViewBlogTag slug ->
    HH.slot_ _viewBlogPostTag slug BlogPostTag.page slug
  ViewBlogCategory slug ->
    HH.slot_ _viewBlogPostCategory slug BlogPostCategory.page slug
  ViewLinks ->
    HH.slot_ _viewLinks unit LinkView.page unit
  ViewLinkCategory slug ->
    HH.slot_ _viewLinkCategory slug LinkCategoryView.page slug
  page ->
    HH.h1_ [ HH.text $ show page ]
  where
  pageWrapper :: forall w i. HH.HTML w i -> HH.HTML w i
  pageWrapper content = HH.div [ HP.class_ $ H.ClassName "main" ]
    [ content ]

renderAdmin
  :: forall a m
   . ApiRequest m
  => Navigation m
  => FileUpload m
  => Auth m
  => AdminRoute
  -> H.ComponentHTML a Slots m
renderAdmin = case _ of
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
