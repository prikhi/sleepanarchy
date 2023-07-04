{- | Common HTML properties for adding structured Microdata markup to page elements.
-}
module Views.MicroData where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Halogen.HTML as HH
import Router (Route, reverse)

-- | The `itemscope` attribute.
itemScope :: forall r i. HH.IProp r i
itemScope = HH.attr (HH.AttrName "itemscope") ""

-- | Make an `itemprop` attribute.
itemProp :: forall r i. String -> HH.IProp r i
itemProp = HH.attr (HH.AttrName "itemprop")

-- | Make a `content` attribute.
itemContent :: forall r i. String -> HH.IProp r i
itemContent = HH.attr (HH.AttrName "content")

-- | Make a `meta` tag with `itemprop` & `content` attributes.
metaItemProp :: forall w i. String -> String -> HH.HTML w i
metaItemProp prop content =
  HH.meta [ itemProp prop, itemContent content ]

-- | Potential MicroData "things"
data ItemType
  = Blog
  | BlogPosting
  | Organization

derive instance genericItemType :: Generic ItemType _
derive instance eqItemType :: Eq ItemType
instance showItemType :: Show ItemType where
  show = genericShow

-- | Make a `itemtype` attribute for the given type.
itemType :: forall r i. ItemType -> HH.IProp r i
itemType iType = HH.attr (HH.AttrName "itemtype") $
  "https://schema.org/" <> show iType

-- | Make an absolute URL `meta` tag from the Route with our domain prepended.
metaUrl :: forall w i. Route -> HH.HTML w i
metaUrl r = metaItemProp "url" $ "https://sleepanarchy.com" <> reverse r

-- | Standard author item for the SleepAnarchy website.
sleepAnarchyAuthor :: forall w i. HH.HTML w i
sleepAnarchyAuthor = HH.span
  [ itemScope, itemType Organization, itemProp "author" ]
  [ metaItemProp "name" "Sleep Anarchy"
  , metaItemProp "url" "/"
  ]
