module BaseSite (Query(..), component) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Pages.BlogPostList as BlogPostList
import Router (Route(..))
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent as ME

component :: forall i o m. MonadAff m => H.Component Query i o m
component = H.mkComponent
  { initialState: const initial
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery }
  }

data Input a = Goto Route a

data Query a = UpdateRoute Route a

data Action = NavClick Route ME.MouseEvent

type Slots = (homePageSlot :: forall query. H.Slot query Void Unit)

_homePage :: Proxy "homePageSlot"
_homePage = Proxy

type State = { currentPage :: Route }

initial :: State
initial = { currentPage: Home }

handleQuery
  :: forall o m a. Query a -> H.HalogenM State Action Slots o m (Maybe a)
handleQuery = case _ of
  UpdateRoute newRoute next -> do
    H.modify_ (_ { currentPage = newRoute })
    pure $ Just next

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
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

renderPage :: forall a m. MonadAff m => Route -> H.ComponentHTML a Slots m
renderPage = case _ of
  Home ->
    HH.div [ HP.class_ $ H.ClassName "main" ]
      [ HH.slot_ _homePage unit BlogPostList.page unit
      ]
  page ->
    HH.div_ [ HH.h1_ [ HH.text $ show page ] ]
