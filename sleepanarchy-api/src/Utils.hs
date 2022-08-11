{-# LANGUAGE FlexibleContexts #-}
module Utils where

import           Data.Aeson
import           Data.Char                      ( toLower )
import           GHC.Generics


prefixToJSON
    :: (Generic a, GToJSON' Value Zero (Rep a)) => String -> a -> Value
prefixToJSON pfx = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix
    }
  where
    dropPrefix :: String -> String
    dropPrefix = lowerHead . drop (length pfx)
    lowerHead :: String -> String
    lowerHead = \case
        x : xs -> toLower x : xs
        []     -> []
