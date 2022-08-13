module Router where

import Prelude

import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Match (Match, end, lit, root, str)

-- | All possible pages we may render.
data Route
  = Home
  | ViewBlogPost String
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
  NotFound -> "/"

-- | Parser from a location/path to a Route.
router :: Match Route
router =
  root *> oneOf
    [ Home <$ end
    , ViewBlogPost <$> (lit "post" *> str)
    , pure NotFound
    ]
