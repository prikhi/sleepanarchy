{- | View the posts for a given year & month.
-}
module Pages.BlogPostArchive (page, Input) where

import Prelude

import Api (class ApiRequest, ApiError, blogPostArchiveRequest)
import Api.Types (BlogPostList)
import App
  ( class Navigation
  , class PageDataNotifier
  , SEOData
  , mkPageDataNotifierEval
  , newUrl
  )
import Data.Date (Month, Year)
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Router (Route)
import Utils (renderRemoteData)
import Views.Blog (renderBlogPostList, renderBlogSidebar)
import Web.UIEvent.MouseEvent as ME

page
  :: forall q o m
   . ApiRequest m
  => PageDataNotifier m
  => Navigation m
  => H.Component q Input o m
page = H.mkComponent
  { initialState
  , render
  , eval: mkPageDataNotifierEval toSEOData H.defaultEval
      { handleAction = handleAction, initialize = Just Initialize }
  }
  where
  toSEOData :: State -> BlogPostList -> SEOData
  toSEOData st _ =
    let
      dateStr = renderDate st.date
    in
      { pageTitle: "Post Archive: " <> dateStr
      , metaDescription: "Blog posts published in " <> dateStr <> "."
      }

type Input = Tuple Year Month

type State =
  { date :: Tuple Year Month
  , apiData :: RemoteData ApiError BlogPostList
  }

initialState :: Input -> State
initialState date = { date, apiData: NotAsked }

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
    Tuple year month <- H.gets _.date
    H.modify_ _ { apiData = Loading }
    response <- H.lift $ blogPostArchiveRequest year month
    H.modify_ _ { apiData = response }
  Navigate route event ->
    H.lift $ newUrl route $ Just event

render :: forall m. State -> H.ComponentHTML Action () m
render { apiData, date } = renderRemoteData apiData $ \resp ->
  let
    headerText = Just $ "Post Archive: " <> renderDate date
  in
    HH.div [ HP.classes [ H.ClassName "blog-page" ] ]
      [ renderBlogPostList Navigate resp headerText
      , renderBlogSidebar Navigate resp.sidebar
      ]

renderDate :: Tuple Year Month -> String
renderDate (Tuple year month) = show month <> ", " <> show (fromEnum year)
