module Utils where

import Prelude

import Api.Types (ApiDateTime(..))
import Data.DateTime (DateTime(..), day, month, year)
import Data.Array (intercalate)
import Data.Enum (fromEnum)
import Data.String (length)

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
    | length input == desiredLength = input
    | otherwise = padZero desiredLength $ "0" <> input
