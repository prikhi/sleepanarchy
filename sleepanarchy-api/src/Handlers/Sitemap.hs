{-# LANGUAGE RecordWildCards #-}
module Handlers.Sitemap
    ( generateSitemap
    ) where

import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( Max(..) )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime(..)
                                                , toGregorian
                                                )
import           Database.Persist
import           Web.Sitemap.Gen                ( ChangeFrequency(..)
                                                , Sitemap(..)
                                                , SitemapUrl(..)
                                                )

import           App                            ( DB
                                                , runDB
                                                )
import           Models.DB
import           Models.Utils                   ( slugifyTag )

import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T


generateSitemap :: DB m => m Sitemap
generateSitemap = runDB $ do
    publishedPosts <- selectList [BlogPostPublishedAt !=. Nothing] []
    let newestPostDate =
            safeMaximum $ map (blogPostUpdatedAt . entityVal) publishedPosts
        (postUrls, categories, archiveDates, tags) = foldr
            mkBlogPostUrlAndMaps
            ([], M.empty, M.empty, M.empty)
            publishedPosts
        homepageUrl = SitemapUrl
            { sitemapLocation        = baseUrl <> routeToPath Home
            , sitemapLastModified    = newestPostDate
            , sitemapChangeFrequency = Just Daily
            , sitemapPriority        = Just 1
            }
    categoryUrls <-
        map (mkCategoryUrl categories)
            <$> selectList [BlogCategoryId <-. M.keys categories] []
    let archiveUrls = map mkArchiveUrl $ M.assocs archiveDates
        tagUrls     = map mkTagUrl $ M.assocs tags
    links <- selectList [] []
    let linkMap = foldr
            (\(Entity _ link) -> M.insertWith (<>)
                                              (linkParentId link)
                                              (Max $ linkUpdatedAt link)
            )
            M.empty
            links
    linkCategoryUrls <- map (mkLinkCategoryUrl linkMap) <$> selectList [] []
    let newestLinkDate = safeMaximum $ map (linkUpdatedAt . entityVal) links
        linksUrl       = SitemapUrl
            { sitemapLocation        = baseUrl <> routeToPath ViewLinks
            , sitemapLastModified    = newestLinkDate
            , sitemapChangeFrequency = Just Weekly
            , sitemapPriority        = Just 0.5
            }
    return $ Sitemap $ concat
        [ [homepageUrl, linksUrl]
        , postUrls
        , categoryUrls
        , archiveUrls
        , tagUrls
        , linkCategoryUrls
        ]
  where
    baseUrl :: Text
    baseUrl = "https://sleepanarchy.com"
    renderLocation :: Route -> Text
    renderLocation r = baseUrl <> routeToPath r
    safeMaximum :: Ord a => [a] -> Maybe a
    safeMaximum xs | null xs   = Nothing
                   | otherwise = Just $ maximum xs

    -- | Go through each published post, build the SitemapUrl for the post
    -- & extract the category, published Year & Month, and a list of tags.
    --
    -- Insert each extracted value into it's own UpdatedMap so we can have
    -- 'sitemapLastModified` values for each that are based on the updated
    -- time of post's within the grouping.
    mkBlogPostUrlAndMaps
        :: Entity BlogPost
        -> ( [SitemapUrl]
           , UpdatedMap BlogCategoryId
           , UpdatedMap (Integer, Int)
           , UpdatedMap Text
           )
        -> ( [SitemapUrl]
           , UpdatedMap BlogCategoryId
           , UpdatedMap (Integer, Int)
           , UpdatedMap Text
           )
    mkBlogPostUrlAndMaps (Entity _ bp@BlogPost {..}) (urls, categoryMap, archiveMap, tagMap)
        = ( SitemapUrl
                  { sitemapLocation = renderLocation $ ViewBlogPost blogPostSlug
                  , sitemapLastModified    = Just blogPostUpdatedAt
                  , sitemapChangeFrequency = Just Weekly
                  , sitemapPriority        = Nothing
                  }
              : urls
          , insertNewest (blogPostCategoryId, bp) categoryMap
          , insertNewest
              ( (\(y, m, _) -> (y, m)) . toGregorian . utctDay $ fromMaybe
                  blogPostUpdatedAt
                  blogPostPublishedAt
              , bp
              )
              archiveMap
          , insertAllNewest
              (map slugifyTag $ T.split (== ',') blogPostTags, bp)
              tagMap
          )

    mkCategoryUrl
        :: UpdatedMap BlogCategoryId -> Entity BlogCategory -> SitemapUrl
    mkCategoryUrl updatedMap (Entity catId BlogCategory {..}) = SitemapUrl
        { sitemapLocation = renderLocation $ ViewBlogCategory blogCategorySlug
        , sitemapLastModified    = Just
                                   $ maybe blogCategoryUpdatedAt getMax
                                   $ M.lookup catId updatedMap
        , sitemapChangeFrequency = Nothing
        , sitemapPriority        = Just 0.2
        }

    mkArchiveUrl :: ((Integer, Int), Max UTCTime) -> SitemapUrl
    mkArchiveUrl ((year, month), updated) = SitemapUrl
        { sitemapLocation        = renderLocation $ ViewBlogArchive year month
        , sitemapLastModified    = Just $ getMax updated
        , sitemapChangeFrequency = Nothing
        , sitemapPriority        = Just 0.1
        }

    mkTagUrl :: (Text, Max UTCTime) -> SitemapUrl
    mkTagUrl (tagSlug, updated) = SitemapUrl
        { sitemapLocation        = renderLocation $ ViewBlogTag tagSlug
        , sitemapLastModified    = Just $ getMax updated
        , sitemapChangeFrequency = Nothing
        , sitemapPriority        = Just 0.1
        }

    mkLinkCategoryUrl
        :: UpdatedMap LinkCategoryId -> Entity LinkCategory -> SitemapUrl
    mkLinkCategoryUrl updatedMap (Entity catId LinkCategory {..}) = SitemapUrl
        { sitemapLocation = renderLocation $ ViewLinkCategory linkCategorySlug
        , sitemapLastModified    = getMax <$> M.lookup catId updatedMap
        , sitemapChangeFrequency = Nothing
        , sitemapPriority        = Just 0.2
        }

-- | Type corresponding to valid frontend routes. Should match up with the
-- corresponding client type in the @src/Router.purs@ file.
data Route
  = Home
  | ViewBlogPost Text
  | ViewBlogArchive Integer Int
  | ViewBlogTag Text
  | ViewBlogCategory Text
  | ViewLinks
  | ViewLinkCategory Text

-- | Generate the canonical URL for a Route.
routeToPath :: Route -> Text
routeToPath = \case
    Home              -> "/"
    ViewBlogPost slug -> "/post/" <> slug
    ViewBlogArchive year month ->
        "/archive/" <> T.pack (show year) <> "/" <> T.pack (show month)
    ViewBlogTag      slug -> "/tag/" <> slug
    ViewBlogCategory slug -> "/category/" <> slug
    ViewLinks             -> "/links"
    ViewLinkCategory slug -> "/links/" <> slug


-- | Store some value associated with a post along with the maximum updated
-- time of the post.
--
-- This is used for generating last modified fields for sitemap urls based
-- on the latest updated time of posts within some grouping(category, tag,
-- date).
type UpdatedMap a = M.Map a (Max UTCTime)

-- | Insert a key & value into the updated map, keeping the maximum value
-- if the key already exists.
insertNewest :: Ord a => (a, BlogPost) -> UpdatedMap a -> UpdatedMap a
insertNewest (val, BlogPost { blogPostUpdatedAt }) =
    M.insertWith (<>) val (Max blogPostUpdatedAt)

-- | Insert multiple keys corresponding to the same blog post.
insertAllNewest :: Ord a => ([a], BlogPost) -> UpdatedMap a -> UpdatedMap a
insertAllNewest (vals, bp) m = foldr (\v -> insertNewest (v, bp)) m vals
