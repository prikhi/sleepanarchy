module BaseSite (Query(..), component) where

import Prelude

import Api (class ApiRequest)
import App (class GetTime, class Navigation, getToday, newUrl)
import Data.Date (Date, canonicalDate, year)
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Pages.BlogPostList as BlogPostList
import Pages.BlogPostView as BlogPostView
import Router (Route(..), navLinkAttr)
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent as ME

-- | The component representing the entire Halogen app. This has slots for each
-- | distinct page type and switches on the stored Route to determine which to
-- | render. It accepts route updates from outside the Halogen environment.
component
  :: forall i o m
   . GetTime m
  => ApiRequest m
  => Navigation m
  => H.Component Query i o m
component = H.mkComponent
  { initialState: const initial
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
  )

_homePage :: Proxy "homePageSlot"
_homePage = Proxy

_viewBlogPost :: Proxy "viewBlogPostSlot"
_viewBlogPost = Proxy

-- | The base app only cares about the current page & date, all other state is
-- | stored within the various `Page.*` module componets.
type State = { currentPage :: Route, currentDate :: Date }

initial :: State
initial =
  { currentPage: Home
  , currentDate: canonicalDate bottom bottom bottom
  }

handleQuery
  :: forall o m a. Query a -> H.HalogenM State Action Slots o m (Maybe a)
handleQuery = case _ of
  UpdateRoute newRoute next -> do
    H.modify_ (_ { currentPage = newRoute })
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
  => State
  -> H.ComponentHTML Action Slots m
render { currentPage, currentDate } =
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
      if route == currentPage then [ H.ClassName "active" ] else []
    navLink text route =
      HH.a
        ( (navLinkAttr route (NavClick route)) <>
            [ HP.classes (activePageClass route) ]
        )
        [ HH.text text ]
  in
    HH.nav [ HP.class_ $ HH.ClassName "header" ]
      [ HH.div [ HP.class_ $ HH.ClassName "site-logo" ]
          [ navLink "Sleep Anarchy" Home ]
      , HH.div [ HP.class_ $ HH.ClassName "nav" ]
          [ HH.ul_
              [ HH.li_ [ navLink "Home" Home ]
              , HH.li_ [ HH.a_ [ HH.text "Links" ] ]
              ]
          ]
      ]

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
  => Route
  -> H.ComponentHTML a Slots m
renderPage = pageWrapper <<< case _ of
  Home ->
    HH.slot_ _homePage unit BlogPostList.page unit
  ViewBlogPost slug ->
    HH.slot_ _viewBlogPost slug BlogPostView.page slug
  page ->
    HH.h1_ [ HH.text $ show page ]
  where
  pageWrapper :: forall w i. HH.HTML w i -> HH.HTML w i
  pageWrapper content = HH.div [ HP.class_ $ H.ClassName "main" ]
    [ content ]
