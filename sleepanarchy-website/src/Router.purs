module Router where

import Prelude

import Data.Date (Month, Year)
import Data.Either (hush)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semiring.Free (free)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Validation.Semiring (invalid)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routing (match)
import Routing.Match (Match(..), end, int, list, lit, optionalMatch, param, root, str)
import Routing.Match.Error (MatchError(..))
import Routing.Types (RoutePart(..))
import Web.UIEvent.MouseEvent as ME

-- | All possible pages we may render.
data Route
  = Home
  | ViewBlogPost String
  | ViewBlogArchive Year Month
  | ViewBlogTag String
  | ViewBlogCategory String
  | Admin AdminRoute
  | NotFound

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where
  show = genericShow

-- | Generate the canonical URL for a Route.
reverse :: Route -> String
reverse = case _ of
  Home -> "/"
  ViewBlogPost slug -> "/post/" <> slug
  ViewBlogArchive year month -> "/archive/" <> show (fromEnum year) <> "/" <>
    show (fromEnum month)
  ViewBlogTag slug -> "/tag/" <> slugify slug
  ViewBlogCategory slug -> "/category/" <> slug
  Admin adminRoute -> "/admin" <> reverseAdmin adminRoute
  NotFound -> "/"
  where
  slugify :: String -> String
  slugify = String.replaceAll (String.Pattern " ") (String.Replacement "-") >>>
    String.toLower

-- | All possible admin pages we may render.
data AdminRoute
  = Login (Maybe String)
  | Dashboard
  | AdminBlogPostList
  | AdminBlogPostEdit Int
  | AdminBlogPostCreate
  | AdminMediaList (Array String)

derive instance genericAdminRoute :: Generic AdminRoute _
derive instance eqAdminRoute :: Eq AdminRoute
instance showAdminRoute :: Show AdminRoute where
  show = genericShow

reverseAdmin :: AdminRoute -> String
reverseAdmin = case _ of
  Login mbRedirectTo -> "/login" <> case mbRedirectTo of
    Nothing -> ""
    Just redirectTo -> "?redirectTo=" <> redirectTo
  Dashboard -> "/"
  AdminBlogPostList -> "/posts"
  AdminBlogPostEdit postId -> "/posts/" <> show postId
  AdminBlogPostCreate -> "/posts/new"
  AdminMediaList folders -> "/media/" <> String.joinWith "/" folders

-- | Generate the 'href' & 'onClick' attributes for a link to an internal page.
navLinkAttr
  :: forall handlerAction r
   . (Route -> ME.MouseEvent -> handlerAction)
  -> Route
  -> Array
       (HP.IProp (href :: String, onClick :: ME.MouseEvent | r) handlerAction)
navLinkAttr handlerAction route =
  [ HP.href $ reverse route, HE.onClick $ handlerAction route ]

-- | Parse the redirection path for the Login page's optional `redirectTo`
-- | query parameter, defaulting to the Admin Dashboard route.
parseRedirectPath :: Maybe String -> Route
parseRedirectPath mbRedirectPath = fromMaybe (Admin Dashboard) $ mbRedirectPath
  >>= match router
    >>> hush

-- | Parser from a location/path to a Route.
router :: Match Route
router =
  root *> oneOf
    [ Home <$ end
    , ViewBlogPost <$> (lit "post" *> str) <* end
    , ViewBlogArchive <$> (lit "archive" *> enum) <*> enum <* end
    , ViewBlogTag <$> (lit "tag" *> str) <* end
    , ViewBlogCategory <$> (lit "category" *> str) <* end
    , Admin <$> (lit "admin" *> adminRouter)
    , pure NotFound
    ]

adminRouter :: Match AdminRoute
adminRouter =
  oneOf
    [ Login <$> (lit "login" *> redirectParam <* end)
    , Dashboard <$ end
    , AdminBlogPostList <$ lit "posts" <* end
    , AdminBlogPostEdit <$> (lit "posts" *> int) <* end
    , AdminBlogPostCreate <$ lit "posts" <* lit "new" <* end
    , AdminMediaList <$> (map List.toUnfoldable $ lit "media" *> list str) <* end
    ]
  where
  redirectParam :: Match (Maybe String)
  redirectParam =
    optionalMatch $ param "redirectTo"

-- | TODO: make MR to upstream @purescript-routing@ package
enum :: forall a. BoundedEnum a => Match a
enum = Match \route ->
  case route of
    Cons (Path input) rs -> case Int.fromString input >>= toEnum of
      Nothing -> invalid $ free ExpectedInt
      Just res -> pure $ Tuple rs res
    _ ->
      invalid $ free ExpectedInt
