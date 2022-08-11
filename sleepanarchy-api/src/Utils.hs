{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Utils where

import           Control.Lens                   ( (<>~) )
import           Data.Aeson
import           Data.Char                      ( toLower )
import           Data.Function                  ( (&) )
import           Data.Proxy                     ( Proxy(..) )
import           GHC.Generics
import           Servant.API                    ( HasLink )
import           Servant.API.TypeLevel          ( IsIn )
import           Servant.Docs                   ( DocNote(..)
                                                , ExtraInfo
                                                , HasDocs
                                                , defAction
                                                , extraInfo
                                                , notes
                                                )
import           Servant.Docs.Internal.Pretty   ( Pretty
                                                , pretty
                                                )


-- | Generate a 'ToJSON' instance by dropping a prefix from the beginning
-- of some fields & converting to camelCase.
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


-- | Annotate an endpoint with some additional documentation.
mkEndpointNotes
    :: forall endpoint api
     . ( IsIn (Pretty endpoint) (Pretty api)
       , HasLink (Pretty endpoint)
       , HasDocs (Pretty endpoint)
       )
    => (String, [String])
    -> ExtraInfo (Pretty api)
mkEndpointNotes routeDocs =
    extraInfo (pretty $ Proxy @endpoint)
        $   defAction
        &   notes
        <>~ [uncurry DocNote routeDocs]
