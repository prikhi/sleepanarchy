{- | View all links.
-}
module Pages.LinkView (page) where

import Prelude

import Api (class ApiRequest, ApiError, linkListRequest, renderApiError)
import Api.Types (RootLinkCategories)
import App (class Navigation, newUrl, openInNewTab)
import Data.Array (concatMap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Router (Route)
import Views.Link (renderCategoryRows)
import Web.UIEvent.MouseEvent as ME

page :: forall q i o m. ApiRequest m => Navigation m => H.Component q i o m
page = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction, initialize = Just Initialize }
  }

type State =
  { apiData :: Maybe (Either ApiError RootLinkCategories)
  }

initialState :: forall i. i -> State
initialState = const { apiData: Nothing }

data Action
  = Initialize
  | VisitLink String
  | VisitCategory Route ME.MouseEvent

handleAction
  :: forall o m
   . ApiRequest m
  => Navigation m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    response <- H.lift linkListRequest
    H.modify_ _ { apiData = Just response }
  VisitLink slug ->
    H.lift $ openInNewTab $ "/l/" <> slug
  VisitCategory r e ->
    H.lift $ newUrl r (Just e)

render :: forall w. State -> HH.HTML w Action
render st = case st.apiData of
  Nothing ->
    HH.div_ [ HH.text "Loading..." ]
  Just (Left e) ->
    HH.div_ [ HH.text $ "Error making request. " <> renderApiError e ]
  Just (Right resp) ->
    HH.div [ HP.classes [ H.ClassName "link-page" ] ]
      [ HH.h1 [] [ HH.text "Links" ]
      , HH.table []
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "Name" ]
                  , HH.th_ [ HH.text "Description" ]
                  , HH.th_ [ HH.text "Views" ]
                  ]
              ]
          , HH.tbody_ $ concatMap
              ( renderCategoryRows
                  { visitLink: VisitLink, visitCategory: VisitCategory }
                  0
              )
              resp
          ]
      ]
