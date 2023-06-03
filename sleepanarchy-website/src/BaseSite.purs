module BaseSite (Query(..), component) where

import Prelude

import Api (class ApiRequest)
import App
  ( class Auth
  , class FileUpload
  , class GetTime
  , class Markdown
  , class Navigation
  , class PageDataListener
  , class PageDataNotifier
  , PageDataDelayedNotif
  , getToday
  , isLoggedIn
  , killDelayedPageDataNotif
  , newUrl
  , notifyPageDataAfterMs
  , pageDataActionEmitter
  )
import BaseAdmin as BaseAdmin
import Data.Date (Date, canonicalDate, year)
import Data.Enum (fromEnum)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
  => PageDataListener m
  => PageDataNotifier m
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
  -- | Clear the stored previous page, forcing us to render the current page,
  -- | whether the data is loaded or not.
  | ClearPreviousPage

type Slots =
  ( homePageSlot :: forall query. H.Slot query Void Unit
  , viewBlogPostSlot :: forall query. H.Slot query Void String
  , viewBlogArchiveSlot :: forall query. H.Slot query Void BlogPostArchive.Input
  , viewBlogTagSlot :: forall query. H.Slot query Void BlogPostTag.Input
  , viewBlogCategorySlot ::
      forall query. H.Slot query Void BlogPostCategory.Input
  , viewLinksSlot :: forall query. H.Slot query Void Unit
  , viewLinkCategorySlot :: forall query. H.Slot query Void String
  , viewAdminSlot :: forall query. H.Slot query Void Unit
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

_viewLinks :: Proxy "viewLinksSlot"
_viewLinks = Proxy

_viewLinkCategory :: Proxy "viewLinkCategorySlot"
_viewLinkCategory = Proxy

_viewAdmin :: Proxy "viewAdminSlot"
_viewAdmin = Proxy

-- | The base app only cares about the current page & date, all other state is
-- | stored within the various `Page.*` module componets.
type State =
  { currentPage :: Route
  , previousPage :: Maybe Route
  , loadingWait :: Maybe PageDataDelayedNotif
  , currentDate :: Date
  }

initial :: Route -> State
initial route =
  { currentPage: route
  , previousPage: Nothing
  , loadingWait: Nothing
  , currentDate: canonicalDate bottom bottom bottom
  }

handleQuery
  :: forall o m a
   . Auth m
  => PageDataListener m
  => Navigation m
  => Query a
  -> H.HalogenM State Action Slots o m (Maybe a)
handleQuery = case _ of
  UpdateRoute newRoute next -> do
    isAuthed <- H.lift isLoggedIn
    case newRoute of
      Admin (Login mbRedirectPath) ->
        if isAuthed then
          H.lift $ newUrl (parseRedirectPath mbRedirectPath) Nothing
        else
          setPageToNewRoute newRoute
      Admin adminRoute ->
        if not isAuthed then do
          let
            mbRedirectTo =
              if adminRoute == Dashboard then Nothing
              else Just $ reverse newRoute
          H.lift $ newUrl (Admin $ Login mbRedirectTo) Nothing
        else
          setPageToNewRoute newRoute
      _ ->
        setPageToNewRoute newRoute
    pure $ Just next
  where
  -- Kick off the async page data clearer then set the new page & previous
  -- page.
  setPageToNewRoute :: Route -> H.HalogenM State Action Slots o m Unit
  setPageToNewRoute newRoute = do
    delayedNotif <- H.lift $ notifyPageDataAfterMs $ Milliseconds 250.0
    H.modify_
      ( \st -> st
          { currentPage = newRoute
          , previousPage = Just st.currentPage
          , loadingWait = Just delayedNotif
          }
      )

handleAction
  :: forall m o
   . GetTime m
  => Navigation m
  => PageDataListener m
  => Action
  -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    H.lift (pageDataActionEmitter ClearPreviousPage) >>= void <<< H.subscribe
    today <- H.lift getToday
    H.modify_ _ { currentDate = today }
  NavClick route event ->
    H.lift $ newUrl route $ Just event
  ClearPreviousPage -> do
    mbDelayedNotif <- H.gets _.loadingWait
    H.lift $ for_ mbDelayedNotif killDelayedPageDataNotif
    H.modify_ _ { previousPage = Nothing, loadingWait = Nothing }

-- | Render the Header & Page Content.
-- |
-- | If a previousPage page exists, continue rendering it while rendering the new
-- | page as hidden.
render
  :: forall m
   . ApiRequest m
  => Navigation m
  => Auth m
  => FileUpload m
  => PageDataNotifier m
  => Markdown m
  => State
  -> H.ComponentHTML Action Slots m
render { currentPage, previousPage, currentDate } =
  case currentPage of
    Admin adminRoute ->
      let
        prevAdmin = previousPage >>= case _ of
          Admin prevAdminRoute -> Just prevAdminRoute
          _ -> Nothing
        adminInput = { currentPage: adminRoute, previousPage: prevAdmin }
      in
        HH.slot_ _viewAdmin unit BaseAdmin.component adminInput
    _ ->
      HH.div [ HP.id "root" ]
        [ renderHeader $ fromMaybe currentPage previousPage
        , HH.div [ HP.id "main" ] $
            case previousPage of
              Nothing ->
                [ renderPage currentPage ]
              Just prevPage ->
                [ HH.div [ HP.class_ $ H.ClassName "hidden" ]
                    [ renderPage currentPage ]
                , HH.div_ [ renderPage prevPage ]
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
  => PageDataNotifier m
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
