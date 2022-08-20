{- | View the posts for a category.
-}
module Pages.BlogPostCategory (page, Input) where

import Prelude

import Api
  ( class ApiRequest
  , ApiError
  , blogPostCategoryRequest
  , renderApiError
  )
import Api.Types (BlogPostList)
import App (class Navigation, newUrl)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Router (Route)
import Views.Blog (renderBlogPostList, renderBlogSidebar)
import Web.UIEvent.MouseEvent as ME

page :: forall q o m. ApiRequest m => Navigation m => H.Component q Input o m
page = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction, initialize = Just Initialize }
  }

type Input = String

type State =
  { slug :: String
  , title :: Maybe String
  , apiData :: Maybe (Either ApiError BlogPostList)
  }

initialState :: Input -> State
initialState slug = { slug, apiData: Nothing, title: Nothing }

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
    slug <- H.gets _.slug
    response <- H.lift $ blogPostCategoryRequest $ slugify slug
    let
      title =
        hush response >>= _.posts >>> map (_.category >>> _.title) >>>
          Array.head
    H.modify_ _ { apiData = Just response, title = title }
  Navigate route event ->
    H.lift $ newUrl route $ Just event
  where
  slugify :: String -> String
  slugify = String.replaceAll (String.Pattern " ") (String.Replacement "-") >>>
    String.toLower

render :: forall m. State -> H.ComponentHTML Action () m
render { apiData, slug, title } = case apiData of
  Nothing ->
    HH.div_ [ HH.text "Loading..." ]
  Just (Left e) ->
    HH.div_ [ HH.text $ "Error making request. " <> renderApiError e ]
  Just (Right resp) ->
    let
      unslugify =
        String.split (String.Pattern "-")
          >>> mapMaybe
            ( \word ->
                String.uncons word <#> \{ head, tail } ->
                  String.toUpper (String.fromCodePointArray [ head ]) <> tail
            )
          >>> String.joinWith " "
      headerText = Just $ "Category: " <> fromMaybe (unslugify slug) title
    in
      HH.div [ HP.classes [ H.ClassName "blog-page" ] ]
        [ renderBlogPostList Navigate resp headerText
        , renderBlogSidebar Navigate resp.sidebar
        ]