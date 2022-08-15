module Views.Blog where

import Prelude

import Api.Types
  ( BlogArchiveListItem(..)
  , BlogRecentPost
  , BlogSidebar
  )
import Data.Array (groupBy, sortBy)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Enum (fromEnum)
import Data.Function (on)
import Data.Ord.Down (Down(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Router (Route(..), navLinkAttr)
import Web.UIEvent.MouseEvent as ME

renderBlogSidebar
  :: forall w a. (String -> ME.MouseEvent -> a) -> BlogSidebar -> HH.HTML w a
renderBlogSidebar linkAction { archive, recent } =
  HH.div [ HP.classes [ H.ClassName "blog-sidebar" ] ]
    [ renderRecentBlock recent
    , renderArchiveBlock archive
    ]
  where
  postLink :: forall r y. { slug :: String | r } -> String -> HH.HTML y a
  postLink { slug } text =
    HH.a (navLinkAttr (ViewBlogPost slug) $ linkAction slug) [ HH.text text ]

  renderRecentBlock :: Array BlogRecentPost -> HH.HTML w a
  renderRecentBlock items =
    HH.div_
      [ HH.h4_ [ HH.text "Recent Posts" ]
      , HH.ul_ $ map (\i -> HH.li_ [ postLink i i.title ]) items
      ]

  renderArchiveBlock :: forall y i. Array BlogArchiveListItem -> HH.HTML y i
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
    :: forall y i. NonEmptyArray BlogArchiveListItem -> HH.HTML y i
  renderArchiveYear yearItems =
    let
      year = (\(BlogArchiveListItem i) -> i.year) $ NonEmptyArray.head
        yearItems
    in
      HH.div_
        [ HH.h5_ [ HH.text $ show $ fromEnum year ]
        , HH.ul_ $ NonEmptyArray.toArray $ renderArchiveMonth <$> yearItems
        ]

  renderArchiveMonth :: forall y i. BlogArchiveListItem -> HH.HTML y i
  renderArchiveMonth (BlogArchiveListItem bali) =
    HH.li_
      [ HH.text $ show bali.month
      , HH.text " ("
      , HH.text $ show bali.count
      , HH.text ")"
      ]
