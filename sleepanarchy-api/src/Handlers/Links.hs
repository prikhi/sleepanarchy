{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Handlers.Links
    ( getAllLinks
    , getLinkCategory
    , RootLinkCategories(..)
    , LinkCategoryMap(..)
    , LinkDetails(..)
    , redirectToLink
    , RedirectLocation(..)
    , RedirectBody(..)
    ) where

import           Control.Exception.Safe         ( throwM )
import           Control.Monad.Except           ( Except
                                                , runExcept
                                                , throwError
                                                )
import           Data.Aeson                     ( ToJSON(..) )
import           Data.ByteString                ( ByteString )
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Database.Persist               ( (+=.)
                                                , Entity(..)
                                                , SelectOpt(..)
                                                , getBy
                                                , selectList
                                                , update
                                                )
import           GHC.Generics                   ( Generic )
import           Servant                        ( Header
                                                , Headers
                                                , MimeRender
                                                , OctetStream
                                                , ServerError(..)
                                                , ToHttpApiData
                                                , addHeader
                                                , err404
                                                )
import           Servant.Docs                   ( ToSample(..)
                                                , singleSample
                                                )

import           App                            ( DB(..)
                                                , DBThrows(..)
                                                , ThrowsError(..)
                                                )
import           Models.DB
import           Utils                          ( prefixToJSON )

import qualified Data.Map.Strict               as M


-- LIST

-- | All the top-level 'LinkCategory' data.
newtype RootLinkCategories = RootLinkCategories
    { rlcRootCategories :: [LinkCategoryMap]
    }
    deriving (Show, Read, Eq, Ord, Generic)
    deriving newtype (ToJSON)

instance ToSample RootLinkCategories where
    toSamples _ = singleSample $ RootLinkCategories
        [snd $ head $ toSamples $ Proxy @LinkCategoryMap]

-- | Recursive data for a 'LinkCategory' with all it's direct 'Link's and
-- it's child categories.
data LinkCategoryMap = LinkCategoryMap
    { lcmCategory    :: Text
    -- ^ The category's name
    , lcmSlug        :: Text
    -- ^ The category's slug
    , lcmChildren    :: [LinkCategoryMap]
    -- ^ The category's child categories' details
    , lcmLinks       :: [LinkDetails]
    -- ^ The category's direct child links
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON LinkCategoryMap where
    toJSON = prefixToJSON "lcm"

instance ToSample LinkCategoryMap where
    toSamples _ = singleSample LinkCategoryMap
        { lcmCategory    = "Root Category"
        , lcmSlug        = "root-category"
        , lcmChildren    =
            [ LinkCategoryMap
                { lcmCategory    = "A Nested Category"
                , lcmSlug        = "a-nested-category"
                , lcmChildren    = []
                , lcmLinks       = [ LinkDetails { ldTitle = "Another Link"
                                                 , ldSlug        = "932d"
                                                 , ldDescription = ""
                                                 , ldViews       = 0
                                                 }
                                   ]
                }
            , LinkCategoryMap
                { lcmCategory    = "Some Empty Category"
                , lcmSlug        = "some-empty-category"
                , lcmChildren    = []
                , lcmLinks       = []
                }
            ]
        , lcmLinks = [ LinkDetails { ldTitle       = "Child Link 1"
                                   , ldSlug        = "xUd2"
                                   , ldDescription = "Some descriptive text"
                                   , ldViews       = 42
                                   }
                     , LinkDetails { ldTitle = "Page title or something better"
                                   , ldSlug        = "Iwn2"
                                   , ldDescription = "Whats this page about"
                                   , ldViews       = 9001
                                   }
                     ]
        }

-- | Data for rendering & redirecting to a specific link.
data LinkDetails = LinkDetails
    { ldTitle       :: Text
    -- ^ The link's name.
    , ldSlug        :: Text
    -- ^ The link's slug, used for redirecting to the link's target
    -- location.
    , ldDescription :: Text
    -- ^ A description about the link location's content.
    , ldViews       :: Int
    -- ^ The number of times we've redirect to the link.
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON LinkDetails where
    toJSON = prefixToJSON "ld"


-- | Fetch all categories that have a link somewhere under them. Any
-- categories that have no links are removing.
--
-- Returns a tree-like recursive structure where the top-level category
-- details are returned with children nested inside them. Each level of
-- categories & links are ordered by their name.
getAllLinks :: DB m => m RootLinkCategories
getAllLinks = runDB $ do
    links      <- selectList [] [Asc LinkTitle]
    categories <- selectList [] [Asc LinkCategoryTitle]
    let linkMap                       = mkLinkMap links
        (rootCategories, categoryMap) = mkCategoryMap categories
    return . RootLinkCategories $ assembleFinalMap (linkMap, categoryMap)
                                                   rootCategories
  where
    mkLinkMap :: [Entity Link] -> M.Map LinkCategoryId [LinkDetails]
    mkLinkMap = foldr
        (\link lMap -> M.insertWith (++)
                                    (linkParentId $ entityVal link)
                                    [mkLinkDetails link]
                                    lMap
        )
        M.empty
    mkLinkDetails :: Entity Link -> LinkDetails
    mkLinkDetails (Entity _ Link {..}) = LinkDetails
        { ldTitle       = linkTitle
        , ldSlug        = linkSlug
        , ldDescription = linkDescription
        , ldViews       = linkViews
        }
    mkCategoryMap
        :: [Entity LinkCategory]
        -> ( [Entity LinkCategory]
           , M.Map LinkCategoryId [Entity LinkCategory]
           )
    mkCategoryMap = foldr
        (\linkCategory (roots, lcMap) ->
            case linkCategoryParentId $ entityVal linkCategory of
                Nothing -> (linkCategory : roots, lcMap)
                Just parentId ->
                    (roots, M.insertWith (++) parentId [linkCategory] lcMap)
        )
        ([], M.empty)
    assembleFinalMap
        :: ( M.Map LinkCategoryId [LinkDetails]
           , M.Map LinkCategoryId [Entity LinkCategory]
           )
        -> [Entity LinkCategory]
        -> [LinkCategoryMap]
    assembleFinalMap ctx@(linkMap, categoryMap) =
        mapMaybe $ \(Entity lcId LinkCategory {..}) ->
            let links    = M.lookup lcId linkMap
                children = assembleFinalMap ctx <$> M.lookup lcId categoryMap
            in  case (links, children) of
                    (Nothing, Nothing) -> Nothing
                    (Nothing, Just []) -> Nothing
                    _                  -> Just LinkCategoryMap
                        { lcmCategory    = linkCategoryTitle
                        , lcmSlug        = linkCategorySlug
                        , lcmChildren    = fromMaybe [] children
                        , lcmLinks       = fromMaybe [] links
                        }

-- | Fetch a categroy's details by it's slug. Returns any links and
-- categories nested under the category.
--
-- Throws a 404 if there is no mathcing category or the category & it's
-- nested categories have no 'Link's.
getLinkCategory :: (Monad m, DB m, ThrowsError m) => Text -> m LinkCategoryMap
getLinkCategory targetSlug = do
    RootLinkCategories rootCategories <- getAllLinks
    case runExcept $ findCategory rootCategories of
        Left  lcMap -> return lcMap
        Right ()    -> serverError err404
            { errBody = "getLinkCategory: Category not found"
            }
  where
    findCategory :: [LinkCategoryMap] -> Except LinkCategoryMap ()
    findCategory = mapM_ $ \lcMap -> if lcmSlug lcMap == targetSlug
        then throwError lcMap
        else findCategory $ lcmChildren lcMap


-- REDIRECT

-- | The location we redirect to.
newtype RedirectLocation = RedirectLocation
    { fromRedirectLocation :: Text
    }
    deriving (Show, Read, Eq, Ord, Generic)
    deriving newtype (ToHttpApiData)

instance ToSample RedirectLocation where
    toSamples _ = singleSample $ RedirectLocation "https://www.google.com/"

-- | The response body contents when we redirect. Always matches the
-- expected body for a 302 response: @"Found"@.
newtype RedirectBody = RedirectBody
    { fromRedirectBody :: ByteString
    }
    deriving (Show, Read, Eq, Ord, Generic)
    deriving newtype (MimeRender OctetStream)

instance ToSample RedirectBody where
    toSamples _ = singleSample $ RedirectBody "Found"

-- | Return a 302 response redirecting to the location for the 'Link' with
-- the given slug.
--
-- Throws a 404 if no 'Link' matches the slug.
redirectToLink
    :: (Monad m, DBThrows m)
    => Text
    -> m (Headers '[Header "Location" RedirectLocation] RedirectBody)
redirectToLink linkSlug = do
    redirectUrl <- runDBThrow $ do
        getBy (UniqueLink linkSlug) >>= \case
            Nothing ->
                throwM err404 { errBody = "redirectToLink: Link not found" }
            Just (Entity linkId link) ->
                update linkId [LinkViews +=. 1] >> return (linkLink link)
    return $ addHeader (RedirectLocation redirectUrl) $ RedirectBody "Found"
