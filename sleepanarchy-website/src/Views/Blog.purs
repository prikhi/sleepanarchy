{- | Helper rendering functions for the Blog pages.
-}
module Views.Blog where

import Prelude

import Api.Types
  ( ApiDateTime
  , BlogArchiveListItem(..)
  , BlogCategory
  , BlogPostList
  , BlogPostListItem
  , BlogSidebar
  , BlogSidebarCategory
  , BlogTag
  )
import Data.Array (foldMap, groupBy, intersperse, null, sortBy)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap)
import Data.Enum (fromEnum)
import Data.Function (on)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Ord.Down (Down(..))
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Html.Renderer.Halogen as RH
import Router (Route(..), navLinkAttr)
import Utils (showDate)
import Views.MicroData as MD
import Web.UIEvent.MouseEvent as ME

-- | Render a list of blog posts with an optional page heading.
renderBlogPostList
  :: forall w a
   . (Route -> ME.MouseEvent -> a)
  -> BlogPostList
  -> Maybe String
  -> HH.HTML w a
renderBlogPostList linkAction listData pageHeader =
  HH.div
    [ HP.classes [ H.ClassName "post-list" ]
    , MD.itemScope
    , MD.itemType MD.Blog
    ]
    $ withHeader
    $ handleBlank
    $ intersperse (HH.hr [ HP.classes [ H.ClassName "post-separator" ] ])
    $ map renderBlogPost listData.posts
  where
  withHeader :: forall x i. Array (HH.HTML x i) -> Array (HH.HTML x i)
  withHeader = case pageHeader of
    Nothing -> identity
    Just headerText -> \content ->
      [ HH.h1_ [ HH.text headerText ] ] <> content

  handleBlank :: forall x i. Array (HH.HTML x i) -> Array (HH.HTML x i)
  handleBlank items
    | null items =
        [ HH.p_ [ HH.text "No posts found." ] ]
    | otherwise =
        items

  renderBlogPost :: forall x. BlogPostListItem -> HH.HTML x a
  renderBlogPost bpld =
    HH.div
      [ MD.itemProp "blogPost"
      , MD.itemScope
      , MD.itemType MD.BlogPosting
      ]
      [ HH.h2 [ HP.classes [ H.ClassName "post-title" ] ]
          [ postLink bpld bpld.title ]
      , renderPostMeta linkAction bpld
      , HH.p
          [ HP.classes [ H.ClassName "post-description" ]
          , MD.itemProp "description"
          ]
          [ RH.render_ bpld.description ]
      , renderTagList linkAction bpld.tags
      , renderReadMore bpld
      , MD.sleepAnarchyAuthor
      ]

  postLink :: forall r x. { slug :: String | r } -> String -> HH.HTML x a
  postLink { slug } text =
    HH.a (navLinkAttr linkAction (ViewBlogPost slug))
      [ HH.span [ MD.itemProp "headline" ] [ HH.text text ]
      , MD.metaUrl $ ViewBlogPost slug
      ]

  renderReadMore :: forall r x. { slug :: String | r } -> HH.HTML x a
  renderReadMore { slug } =
    HH.small_
      [ HH.a (navLinkAttr linkAction (ViewBlogPost slug))
          [ HH.text "Read More" ]
      ]

-- | Render the sidebar for the Blog pages.
renderBlogSidebar
  :: forall w a. (Route -> ME.MouseEvent -> a) -> BlogSidebar -> HH.HTML w a
renderBlogSidebar linkAction { archive, recent, tags, categories } =
  HH.div [ HP.classes [ H.ClassName "blog-sidebar" ] ]
    [ recentBlock
    , archiveBlock
    , categoryBlock
    , tagBlock
    ]
  where
  recentBlock :: HH.HTML w a
  recentBlock =
    HH.div_
      [ HH.h4_ [ HH.text "Recent Posts" ]
      , HH.ul_ $ map (\i -> HH.li_ [ postLink i i.title ]) recent
      ]

  postLink :: forall r y. { slug :: String | r } -> String -> HH.HTML y a
  postLink { slug } text =
    HH.a (navLinkAttr linkAction $ ViewBlogPost slug)
      [ HH.text text ]

  archiveBlock :: forall y. HH.HTML y a
  archiveBlock =
    let
      groupedItems = archive
        # sortBy
            (comparing \(BlogArchiveListItem i) -> Down $ Tuple i.year i.month)
        # groupBy (on (==) \(BlogArchiveListItem i) -> i.year)
    in
      HH.div_
        [ HH.h4_ [ HH.text "Archive" ]
        , HH.div_ $ map renderArchiveYear groupedItems
        ]

  renderArchiveYear
    :: forall y. NonEmptyArray BlogArchiveListItem -> HH.HTML y a
  renderArchiveYear yearItems =
    let
      year = (\(BlogArchiveListItem i) -> i.year) $ NonEmptyArray.head
        yearItems
    in
      HH.div_
        [ HH.h5_ [ HH.text $ show $ fromEnum year ]
        , HH.ul_ $ NonEmptyArray.toArray $ renderArchiveMonth <$> yearItems
        ]

  renderArchiveMonth :: forall y. BlogArchiveListItem -> HH.HTML y a
  renderArchiveMonth (BlogArchiveListItem bali) =
    HH.li_
      [ HH.a (navLinkAttr linkAction $ ViewBlogArchive bali.year bali.month)
          [ HH.text $ show bali.month ]
      , HH.text " ("
      , HH.text $ show bali.count
      , HH.text ")"
      ]

  categoryBlock :: forall y. HH.HTML y a
  categoryBlock =
    HH.div_
      [ HH.h4_ [ HH.text "Categories" ]
      , HH.ul [ HP.classes [ H.ClassName "blog-categories" ] ]
          $ map renderCategory categories
      ]

  renderCategory :: forall y. BlogSidebarCategory -> HH.HTML y a
  renderCategory { title, slug, count } =
    HH.li_
      [ HH.a (navLinkAttr linkAction $ ViewBlogCategory slug) [ HH.text title ]
      , HH.text $ " (" <> show count <> ")"
      ]

  tagBlock :: forall y. HH.HTML y a
  tagBlock
    | null tags = HH.text ""
    | otherwise =
        let
          countRange = bimap unwrap unwrap $ foldMap
            ((Min &&& Max) <<< _.count)
            tags
        in
          HH.div_
            [ HH.h4_ [ HH.text "Tags" ]
            , HH.ul [ HP.classes [ H.ClassName "blog-tag-list" ] ] $ map
                (renderTag $ makeTagFontSize countRange)
                tags
            ]

  -- | We create a linear scale from min -> max counts for each tag to min ->
  -- | max font size, then use the tags count to determine it's font size in
  -- | @rem@ units.
  makeTagFontSize :: Tuple Int Int -> Int -> String
  makeTagFontSize (Tuple minCount maxCount) tagCount =
    let
      -- shift the range of counts so it starts at zero
      range = maxCount - minCount
      -- shift the count so the lowest count is at zero
      normalizedCount = tagCount - minCount
      -- find the placement in the range of the tag's count
      ratio = (toNumber normalizedCount) / (toNumber range)
      -- these get translated to rem units.
      minFontSize = 0.75
      maxFontSize = 2.25
      -- this is the zero-based range of font sizes times the tags position in
      -- the count range. the lowest count will have a dynamic size of 0.
      dynamicFontSize = (maxFontSize - minFontSize) * ratio
      -- add the minimum font size to get the final size
      fontSize = dynamicFontSize + minFontSize
    in
      -- render the calculated size with @rem@ units.
      show fontSize <> "rem;"

  renderTag :: forall y. (Int -> String) -> BlogTag -> HH.HTML y a
  renderTag mkFontSize { tag, count } =
    let
      fontSize = mkFontSize count
    in
      HH.li [ HP.classes [ H.ClassName "blog-tag" ] ]
        [ HH.a
            ( navLinkAttr linkAction (ViewBlogTag tag) <>
                [ HP.style $ "font-size: " <> fontSize <> "; height: " <>
                    fontSize
                ]
            )
            [ HH.text tag ]
        , HH.text $ " (" <> show count <> ")"
        ]

-- | Render the Published date for a post. Include the updated date if it has
-- | been updated since being published.
renderPostMeta
  :: forall r w a
   . (Route -> ME.MouseEvent -> a)
  -> { publishedAt :: ApiDateTime
     , updatedAt :: ApiDateTime
     , category :: BlogCategory
     | r
     }
  -> HH.HTML w a
renderPostMeta navigateAction post = HH.div
  [ HP.classes [ H.ClassName "post-meta" ] ]
  [ HH.text $ "Posted in "
  , HH.a (navLinkAttr navigateAction (ViewBlogCategory post.category.slug))
      [ HH.span [ MD.itemProp "articleSection" ] [ HH.text post.category.title ]
      ]
  , HH.text $ " on "
  , HH.span [ MD.itemProp "datePublished" ]
      [ HH.text $ showDate post.publishedAt ]
  , if post.publishedAt < post.updatedAt then
      HH.span_
        [ HH.text $ " | Updated on "
        , HH.span [ MD.itemProp "dateModified" ]
            [ HH.text $ showDate post.updatedAt ]

        ]
    else HH.text ""
  ]

-- | Render a list of tags for a post.
renderTagList
  :: forall w a. (Route -> ME.MouseEvent -> a) -> Array String -> HH.HTML w a
renderTagList navigateAction tags
  | null tags = HH.text ""
  | otherwise =
      HH.small [ HP.classes [ H.ClassName "blog-tag-list" ] ]
        [ HH.text "Tags:"
        , HH.ul_ $ map
            ( \t -> HH.li_
                [ HH.a (navLinkAttr navigateAction $ ViewBlogTag t)
                    [ HH.span [ MD.itemProp "keywords" ] [ HH.text t ] ]
                ]
            )
            tags
        ]
