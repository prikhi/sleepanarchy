{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Utils where

import           Control.Lens                   ( (<>~) )
import           Data.Aeson                     ( defaultOptions
                                                , fieldLabelModifier
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           Data.Aeson.Types               ( GFromJSON
                                                , GToJSON'
                                                , Parser
                                                , Value
                                                , Zero
                                                )
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
-- of fields & converting to camelCase.
prefixToJSON
    :: (Generic a, GToJSON' Value Zero (Rep a)) => String -> a -> Value
prefixToJSON pfx = genericToJSON defaultOptions
    { fieldLabelModifier = dropJSONFieldPrefix pfx
    }

-- | Generate a 'FromJSON' instance by adding the prefix to fields & fixing
-- the casing.
prefixParseJSON
    :: (Generic a, GFromJSON Zero (Rep a)) => String -> Value -> Parser a
prefixParseJSON pfx = genericParseJSON defaultOptions
    { fieldLabelModifier = dropJSONFieldPrefix pfx
    }

dropJSONFieldPrefix :: String -> String -> String
dropJSONFieldPrefix pfx = lowerHead . drop (length pfx)
  where
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
