{- | View the list of blog posts.
-}
module Pages.BlogPostList (page) where

import Prelude

import Api (class ApiRequest, blogPostListRequest)
import Api.Types
  ( BlogArchiveListItem(..)
  , BlogPostList
  , BlogPostListItem
  , BlogRecentPost
  , BlogSidebar
  )
import App (class Navigation, newUrl)
import Data.Array (groupBy, intersperse, sortBy)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Ord.Down (Down(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Router (Route(..), navLinkAttr)
import Utils (showDate)
import Web.UIEvent.MouseEvent as ME

page :: forall q i o m. ApiRequest m => Navigation m => H.Component q i o m
page =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize }
    }

type State =
  { apiData :: Maybe (Either String BlogPostList)
  }

initialState :: forall i. i -> State
initialState _ = { apiData: Nothing }

data Action
  = Initialize
  | ViewPost String ME.MouseEvent

handleAction
  :: forall o m
   . ApiRequest m
  => Navigation m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    response <- H.lift blogPostListRequest
    H.modify_ _ { apiData = Just response }
  ViewPost slug event ->
    H.lift $ newUrl (ViewBlogPost slug) $ Just event

render :: forall m. State -> H.ComponentHTML Action () m
render = _.apiData >>> case _ of
  Nothing ->
    HH.div_ [ HH.text "Loading..." ]
  Just (Left e) ->
    HH.div_ [ HH.text $ "Error making request: " <> e ]
  Just (Right resp) ->
    HH.div [ HP.classes [ H.ClassName "blog-page" ] ]
      [ HH.div [ HP.classes [ H.ClassName "post-list" ] ]
          $ intersperse (HH.hr [ HP.classes [ H.ClassName "post-separator" ] ])
          $ map renderBlogPost resp.posts
      , renderBlogSidebar resp.sidebar
      ]
  where
  renderBlogPost :: forall w. BlogPostListItem -> HH.HTML w Action
  renderBlogPost bpld =
    HH.div_
      [ HH.h2 [ HP.classes [ H.ClassName "post-title" ] ]
          [ postLink bpld bpld.title ]
      , HH.div
          [ HP.classes [ H.ClassName "post-meta" ] ]
          [ HH.text $ "Posted on " <> showDate bpld.publishedAt
          , if bpld.publishedAt /= bpld.updatedAt then
              HH.text $ " | Updated on " <> showDate bpld.updatedAt
            else HH.text ""
          ]
      , HH.p [ HP.classes [ H.ClassName "post-description" ] ]
          [ HH.text bpld.description ]
      , HH.small_ [ postLink bpld "Read More" ]
      ]

renderBlogSidebar :: forall w. BlogSidebar -> HH.HTML w Action
renderBlogSidebar { archive, recent } =
  HH.div [ HP.classes [ H.ClassName "blog-sidebar" ] ]
    [ renderRecentBlock recent
    , renderArchiveBlock archive
    ]
  where
  renderRecentBlock :: Array BlogRecentPost -> HH.HTML w Action
  renderRecentBlock items =
    HH.div_
      [ HH.h4_ [ HH.text "Recent Posts" ]
      , HH.ul_ $ map (\i -> HH.li_ [ postLink i i.title ]) items
      ]

  renderArchiveBlock :: forall i. Array BlogArchiveListItem -> HH.HTML w i
  renderArchiveBlock items =
    let
      groupedItems = items
        # sortBy
            (comparing \(BlogArchiveListItem i) -> Down $ Tuple i.year i.month)
        # groupBy (on (==) \(BlogArchiveListItem i) -> i.year)
    in
      HH.div_
        [ HH.h4_ [ HH.text "Archive" ]
        , HH.div_ $ map renderArchiveYear groupedItems
        ]

  renderArchiveYear
    :: forall i. NonEmptyArray BlogArchiveListItem -> HH.HTML w i
  renderArchiveYear yearItems =
    let
      year = (\(BlogArchiveListItem i) -> i.year) $ NonEmptyArray.head
        yearItems
    in
      HH.div_
        [ HH.h5_ [ HH.text $ show $ fromEnum year ]
        , HH.ul_ $ NonEmptyArray.toArray $ renderArchiveMonth <$> yearItems
        ]

  renderArchiveMonth :: forall i. BlogArchiveListItem -> HH.HTML w i
  renderArchiveMonth (BlogArchiveListItem bali) =
    HH.li_
      [ HH.text $ show bali.month
      , HH.text " ("
      , HH.text $ show bali.count
      , HH.text ")"
      ]

postLink :: forall r w. { slug :: String | r } -> String -> HH.HTML w Action
postLink { slug } text =
  HH.a (navLinkAttr (ViewBlogPost slug) $ ViewPost slug) [ HH.text text ]
