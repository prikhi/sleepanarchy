module Router where

import Prelude

import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routing.Match (Match, end, lit, root, str)
import Web.UIEvent.MouseEvent as ME

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

-- | Generate the 'href' & 'onClick' attributes for a link to an internal page.
navLinkAttr
  :: forall handlerAction r
   . Route
  -> (ME.MouseEvent -> handlerAction)
  -> Array
       (HP.IProp (href :: String, onClick :: ME.MouseEvent | r) handlerAction)
navLinkAttr route handlerAction =
  [ HP.href $ reverse route, HE.onClick handlerAction ]

-- | Parser from a location/path to a Route.
router :: Match Route
router =
  root *> oneOf
    [ Home <$ end
    , ViewBlogPost <$> (lit "post" *> str)
    , pure NotFound
    ]
