module Router where

import Prelude

import Data.Date (Month, Year)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Semiring.Free (free)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Validation.Semiring (invalid)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routing.Match (Match(..), end, lit, root, str)
import Routing.Match.Error (MatchError(..))
import Routing.Types (RoutePart(..))
import Web.UIEvent.MouseEvent as ME

-- | All possible pages we may render.
data Route
  = Home
  | ViewBlogPost String
  | ViewBlogArchive Year Month
  | ViewBlogTag String
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
  NotFound -> "/"
  where
  slugify :: String -> String
  slugify = String.replaceAll (String.Pattern " ") (String.Replacement "-") >>>
    String.toLower

-- | Generate the 'href' & 'onClick' attributes for a link to an internal page.
navLinkAttr
  :: forall handlerAction r
   . (Route -> ME.MouseEvent -> handlerAction)
  -> Route
  -> Array
       (HP.IProp (href :: String, onClick :: ME.MouseEvent | r) handlerAction)
navLinkAttr handlerAction route =
  [ HP.href $ reverse route, HE.onClick $ handlerAction route ]

-- | Parser from a location/path to a Route.
router :: Match Route
router =
  root *> oneOf
    [ Home <$ end
    , ViewBlogPost <$> (lit "post" *> str) <* end
    , ViewBlogArchive <$> (lit "archive" *> enum) <*> enum <* end
    , ViewBlogTag <$> (lit "tag" *> str) <* end
    , pure NotFound
    ]

-- | TODO: make MR to upstream @purescript-routing@ package
enum :: forall a. BoundedEnum a => Match a
enum = Match \route ->
  case route of
    Cons (Path input) rs -> case Int.fromString input >>= toEnum of
      Nothing -> invalid $ free ExpectedInt
      Just res -> pure $ Tuple rs res
    _ ->
      invalid $ free ExpectedInt
