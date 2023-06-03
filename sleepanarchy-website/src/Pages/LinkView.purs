{- | View all links.
-}
module Pages.LinkView (page) where

import Prelude

import Api (class ApiRequest, ApiError, linkListRequest)
import Api.Types (RootLinkCategories)
import App
  ( class Navigation
  , class PageDataNotifier
  , mkPageDataNotifierEval
  , newUrl
  , openInNewTab
  )
import Data.Array (concatMap)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Router (Route)
import Utils (renderRemoteData)
import Views.Link (renderCategoryRows)
import Web.UIEvent.MouseEvent as ME

page
  :: forall q i o m
   . ApiRequest m
  => PageDataNotifier m
  => Navigation m
  => H.Component q i o m
page = H.mkComponent
  { initialState
  , render
  , eval: mkPageDataNotifierEval H.defaultEval
      { handleAction = handleAction, initialize = Just Initialize }
  }

type State =
  { apiData :: RemoteData ApiError RootLinkCategories
  }

initialState :: forall i. i -> State
initialState = const { apiData: NotAsked }

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
    H.modify_ _ { apiData = Loading }
    response <- H.lift linkListRequest
    H.modify_ _ { apiData = response }
  VisitLink slug ->
    H.lift $ openInNewTab $ "/l/" <> slug
  VisitCategory r e ->
    H.lift $ newUrl r (Just e)

render :: forall w. State -> HH.HTML w Action
render st = renderRemoteData st.apiData $ \resp ->
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
