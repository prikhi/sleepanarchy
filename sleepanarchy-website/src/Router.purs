module Router where

import Prelude

import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Match (Match, end, lit, root, str)

data Route
  = Home
  | ViewBlogPost String
  | NotFound

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show = genericShow

reverse :: Route -> String
reverse = case _ of
  Home -> "/"
  ViewBlogPost slug -> "/post/" <> slug
  NotFound -> "/"

router :: Match Route
router =
  root *> oneOf
    [ Home <$ end
    , ViewBlogPost <$> (lit "post" *> str)
    , pure NotFound
    ]
