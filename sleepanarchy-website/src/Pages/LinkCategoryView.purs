{- View links for a specific category.
-}
module Pages.LinkCategoryView (page) where

import Prelude

import Api (class ApiRequest, ApiError, linkCategoryRequest)
import Api.Types (LinkCategoryMap(..))
import App (class Navigation, newUrl, openInNewTab)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Router (Route)
import Utils (renderRemoteData)
import Views.Link (renderCategoryRows)
import Web.UIEvent.MouseEvent as ME

page :: forall q o m. ApiRequest m => Navigation m => H.Component q String o m
page = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction, initialize = Just Initialize }
  }

type State =
  { apiData :: RemoteData ApiError LinkCategoryMap
  , slug :: String
  }

initialState :: String -> State
initialState slug = { apiData: NotAsked, slug }

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
    slug <- H.gets _.slug
    H.modify_ _ { apiData = Loading }
    response <- H.lift $ linkCategoryRequest slug
    H.modify_ _ { apiData = response }
  VisitLink slug ->
    H.lift $ openInNewTab $ "/l/" <> slug
  VisitCategory r e ->
    H.lift $ newUrl r (Just e)

render :: forall w. State -> HH.HTML w Action
render st = renderRemoteData st.apiData $ \(LinkCategoryMap lcm) ->
  HH.div [ HP.classes [ H.ClassName "link-page" ] ]
    [ HH.h1 [] [ HH.text $ "Links: " <> lcm.category ]
    , HH.table []
        [ HH.thead_
            [ HH.tr_
                [ HH.th_ [ HH.text "Name" ]
                , HH.th_ [ HH.text "Description" ]
                , HH.th_ [ HH.text "Views" ]
                ]
            ]
        , HH.tbody_ $ renderCategoryRows
            { visitLink: VisitLink, visitCategory: VisitCategory }
            0
            (LinkCategoryMap lcm)
        ]
    ]
