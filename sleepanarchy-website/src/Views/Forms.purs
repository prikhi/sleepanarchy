module Views.Forms where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

mkInput
  :: forall initial editForm w a
   . initial
  -> editForm
  -> String
  -> Maybe String
  -> (initial -> String)
  -> (editForm -> Maybe String)
  -> (String -> a)
  -> HH.HTML w a
mkInput initial formData label mbHelp selector mbSelector onInput =
  HH.label_
    [ HH.div_ [ HH.text label ]
    , maybe (HH.text "") (\t -> HH.small_ [ HH.text t ]) mbHelp
    , HH.input
        [ HP.value (fromMaybe (selector initial) (mbSelector formData))
        , HE.onValueInput onInput
        ]
    ]

mkTextArea
  :: forall initial editForm w a
   . initial
  -> editForm
  -> String
  -> Maybe String
  -> (initial -> String)
  -> (editForm -> Maybe String)
  -> (String -> a)
  -> HH.HTML w a
mkTextArea initial formData label mbHelp selector mbSelector onInput =
  let
    rows = (_ + 2) <<< length <<< String.split (String.Pattern "\n") $ selector
      initial
  in
    HH.label_
      [ HH.div_ [ HH.text label ]
      , maybe (HH.text "") (\t -> HH.small_ [ HH.text t ]) mbHelp
      , HH.textarea
          [ HE.onValueInput onInput
          , HP.rows rows
          , HP.value $ fromMaybe (selector initial) (mbSelector formData)
          ]
      ]

mkCheckbox
  :: forall initial editForm w a
   . initial
  -> editForm
  -> String
  -> Maybe String
  -> (initial -> Boolean)
  -> (editForm -> Maybe Boolean)
  -> (Boolean -> a)
  -> HH.HTML w a
mkCheckbox initial formData label mbHelp selector mbSelector onCheck =
  HH.label [ HP.classes [ H.ClassName "inline-input" ] ]
    [ HH.div_ [ HH.text label ]
    , maybe (HH.text "") (\t -> HH.small_ [ HH.text t ]) mbHelp
    , HH.input
        [ HP.checked $ fromMaybe (selector initial) (mbSelector formData)
        , HE.onChecked onCheck
        , HP.type_ HP.InputCheckbox
        ]
    ]

mkSelect
  :: forall initial editForm item ident w a
   . Show ident
  => Eq ident
  => initial
  -> editForm
  -> String
  -> Maybe String
  -> (initial -> ident)
  -> (editForm -> Maybe ident)
  -> Array item
  -> (item -> { id :: ident, text :: String })
  -> (String -> a)
  -> HH.HTML w a
mkSelect
  initial
  formData
  label
  mbHelp
  selector
  mbSelector
  items
  toOption
  onChange =
  HH.label [ HP.classes [ H.ClassName "inline-input" ] ]
    [ HH.div_ [ HH.text label ]
    , maybe (HH.text "") (\t -> HH.small_ [ HH.text t ]) mbHelp
    , HH.select [ HE.onValueChange onChange ]
        $ map
            ( toOption >>> \{ id, text } -> HH.option
                [ HP.value $ show id
                , HP.selected $
                    fromMaybe (selector initial)
                      (mbSelector formData)
                      == id
                ]
                [ HH.text text ]
            )
            items
    ]

mkSubmit :: forall w a. String -> HH.HTML w a
mkSubmit content = HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text content ]
