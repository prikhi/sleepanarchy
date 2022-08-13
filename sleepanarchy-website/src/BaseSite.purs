module BaseSite (Query(..), component) where

import Prelude

import Api (class ApiRequest)
import App (class Navigation)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Pages.BlogPostList as BlogPostList
import Pages.BlogPostView as BlogPostView
import Router (Route(..))
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent as ME

-- | The component representing the entire Halogen app. This has slots for each
-- | distinct page type and switches on the stored Route to determine which to
-- | render. It accepts route updates from outside the Halogen environment.
component
  :: forall i o m. ApiRequest m => Navigation m => H.Component Query i o m
component = H.mkComponent
  { initialState: const initial
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery }
  }

data Query a = UpdateRoute Route a

-- ^ Set the route to the given value.

data Action = NavClick Route ME.MouseEvent

-- ^ Response to a nav menu click by changing the route & url.

type Slots =
  ( homePageSlot :: forall query. H.Slot query Void Unit
  , viewBlogPostSlot :: forall query. H.Slot query Void String
  )

_homePage :: Proxy "homePageSlot"
_homePage = Proxy

_viewBlogPost :: Proxy "viewBlogPostSlot"
_viewBlogPost = Proxy

-- | The base app only cares about the current page, all other state is stored
-- | within the various `Page.*` module componets.
type State = { currentPage :: Route }

initial :: State
initial = { currentPage: Home }

handleQuery
  :: forall o m a. Query a -> H.HalogenM State Action Slots o m (Maybe a)
handleQuery = case _ of
  UpdateRoute newRoute next -> do
    H.modify_ (_ { currentPage = newRoute })
    pure $ Just next

-- | Render the Header & Page Content.
render
  :: forall m
   . ApiRequest m
  => Navigation m
  => State
  -> H.ComponentHTML Action Slots m
render { currentPage } =
  HH.div [ HP.id "root" ]
    [ renderHeader currentPage
    , HH.div [ HP.id "main" ]
        [ renderPage currentPage
        ]
    ]

renderHeader :: forall s m. Route -> H.ComponentHTML Action s m
renderHeader currentPage =
  let
    hereText r = if r == currentPage then HH.text " [HERE]" else HH.text ""
  in
    HH.nav [ HP.class_ $ HH.ClassName "header" ]
      [ HH.div [ HP.class_ $ HH.ClassName "site-logo" ]
          [ HH.text "Sleep Anarchy" ]
      , HH.div [ HP.class_ $ HH.ClassName "nav" ]
          [ HH.ul_
              [ HH.li_ [ HH.text "Home", hereText Home ]
              , HH.li_ [ HH.text "Links" ]
              ]
          ]
      ]

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
