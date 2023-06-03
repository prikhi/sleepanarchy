{-| Helper rendering functions for the Link pages.
-}
module Views.Link where

import Prelude

import Api.Types (LinkCategoryMap(..), LinkDetails)
import Data.Array (concatMap, cons)
import Data.Int (toNumber)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Router (Route(..), navLinkAttr)
import Web.UIEvent.MouseEvent as ME

linkTableHeader :: forall w a. HH.HTML w a
linkTableHeader =
  HH.thead_
    [ HH.tr_
        [ HH.th_ [ HH.text "Name" ]
        , HH.th_ [ HH.text "Description" ]
        , HH.th_ [ HH.text "Views" ]
        ]
    ]

renderCategoryRows
  :: forall w a
   . { visitLink :: String -> a, visitCategory :: Route -> ME.MouseEvent -> a }
  -> Int
  -> LinkCategoryMap
  -> Array (HH.HTML w a)
renderCategoryRows act@{ visitLink, visitCategory } level (LinkCategoryMap lcm) =
  cons
    ( HH.tr
        [ HP.classes [ H.ClassName "category", H.ClassName "clickable" ]
        , HE.onClick (visitCategory $ ViewLinkCategory lcm.slug)
        ]
        [ HH.td [ HP.style $ mkPadding level, HP.colSpan 3 ]
            [ HH.a (navLinkAttr visitCategory $ ViewLinkCategory lcm.slug)
                [ HH.text lcm.category ]
            ]
        ]
    )
    $ (map renderLinkRow lcm.links)
        <> (concatMap (renderCategoryRows act (level + 1)) lcm.children)
  where
  renderLinkRow :: LinkDetails -> HH.HTML w a
  renderLinkRow { title, description, slug, views } =
    HH.tr
      [ HP.classes [ H.ClassName "clickable" ]
      , HE.onClick (const $ visitLink slug)
      ]
      [ HH.td [ HP.style $ mkPadding $ level + 1 ]
          [ HH.a [ HP.href $ "/l/" <> slug, HP.target "_blank" ]
              [ HH.text title ]
          ]
      , HH.td [] [ HH.text description ]
      , HH.td [] [ HH.text $ show views ]
      ]

  mkPadding :: Int -> String
  mkPadding itemLevel =
    "padding-left: " <> show (toNumber itemLevel * 1.25 + 0.5) <> "rem;"
