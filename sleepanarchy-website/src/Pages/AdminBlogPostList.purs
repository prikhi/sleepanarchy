module Pages.AdminBlogPostList (page) where

import Prelude

import Api
  ( class ApiRequest
  , ApiError
  , adminBlogPostListRequest
  , renderApiError
  )
import Api.Types (AdminBlogPostList, AdminBlogPostListItem)
import App (class Navigation, newUrl)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Router (AdminRoute(..), Route(..), navLinkAttr)
import Utils (showDate)
import Web.UIEvent.MouseEvent as ME

page :: forall q i o m. ApiRequest m => Navigation m => H.Component q i o m
page = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction, initialize = Just Initialize }
  }

type State =
  { apiData :: Maybe (Either ApiError AdminBlogPostList)
  }

initialState :: forall i. i -> State
initialState _ = { apiData: Nothing }

data Action
  = Initialize
  | Navigate Route ME.MouseEvent

handleAction
  :: forall o m
   . ApiRequest m
  => Navigation m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    response <- H.lift adminBlogPostListRequest
    -- TODO: if 401, call logout & redirect to login page?
    H.modify_ _ { apiData = Just response }
  Navigate route ev ->
    H.lift $ newUrl route $ Just ev

render :: forall m. State -> H.ComponentHTML Action () m
render = _.apiData >>> case _ of
  Nothing -> HH.div_ [ HH.text "Loading..." ]
  Just (Left e) ->
    HH.div_ [ HH.text $ renderApiError e ]
  Just (Right resp) ->
    HH.div [ HP.classes [ H.ClassName "admin-post-list" ] ]
      [ HH.h1_ [ HH.text "Blog Posts" ]
      , HH.a (navLinkAttr Navigate $ Admin AdminBlogPostCreate)
          [ HH.text "New Blog Post" ]
      , HH.table_
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "ID" ]
                  , HH.th_ [ HH.text "Title" ]
                  , HH.th_ [ HH.text "Category" ]
                  , HH.th [ HP.classes [ H.ClassName "date" ] ]
                      [ HH.text "Created" ]
                  , HH.th [ HP.classes [ H.ClassName "date" ] ]
                      [ HH.text "Published" ]
                  ]
              ]
          , HH.tbody_ $ map tableRow resp.posts
          ]
      ]
  where
  tableRow :: forall w. AdminBlogPostListItem -> HH.HTML w Action
  tableRow post =
    HH.tr_
      [ HH.td_ [ HH.text $ show post.id ]
      , HH.td_
          [ HH.a (navLinkAttr Navigate $ Admin $ AdminBlogPostEdit post.id)
              [ HH.text post.title ]
          ]
      , HH.td_ [ HH.text post.category ]
      , HH.td [ HP.classes [ H.ClassName "date" ] ]
          [ HH.text $ showDate post.created ]
      , HH.td [ HP.classes [ H.ClassName "date" ] ]
          [ HH.text $ maybe "" showDate post.published ]
      ]
