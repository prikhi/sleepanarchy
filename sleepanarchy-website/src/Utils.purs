module Utils where

import Prelude

import Api (ApiError, renderApiError)
import Api.Types (ApiDateTime(..))
import Data.Array (intercalate, mapMaybe)
import Data.DateTime (DateTime(..), day, month, year)
import Data.Enum (fromEnum)
import Data.String as String
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..))

-- | Render a date in `YYYY-MM-DD` format.
showDate :: ApiDateTime -> String
showDate (ApiDateTime (DateTime date _)) =
  intercalate "-"
    [ show $ fromEnum $ year date
    , padZero 2 $ show $ fromEnum $ month date
    , padZero 2 $ show $ fromEnum $ day date
    ]
  where
  padZero :: Int -> String -> String
  padZero desiredLength input
    | String.length input == desiredLength = input
    | otherwise = padZero desiredLength $ "0" <> input

-- | Standardized rendering of a NotAsked, Loading, & Failure states.
renderRemoteData
  :: forall a w i. RemoteData ApiError a -> (a -> HH.HTML w i) -> HH.HTML w i
renderRemoteData resp render = case resp of
  NotAsked ->
    HH.div_ [ HH.text "Implementation bug, page data not fetched!" ]
  Loading ->
    HH.div_ [ HH.text "Loading..." ]
  Failure e ->
    HH.div_ [ HH.text $ "Error making request. " <> renderApiError e ]
  Success respData ->
    render respData

-- | Attempt to turn a slug into a space-separated title-cased string.
unslugify :: String -> String
unslugify =
  String.split (String.Pattern "-")
    >>> mapMaybe
      ( \word ->
          String.uncons word <#> \{ head, tail } ->
            String.toUpper (String.fromCodePointArray [ head ]) <> tail
      )
    >>> String.joinWith " "
