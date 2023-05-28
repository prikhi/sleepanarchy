module Models.Utils
    ( slugify
    , slugifyTag
    ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)

import Data.Text qualified as T


-- | Slugify the title of a DB entity for auto-generating slug fields.
slugify :: Text -> Text
slugify =
    T.intercalate "-"
        . filter (not . T.null)
        . T.words
        . T.toLower
        . replaceInvalid
  where
    replaceInvalid :: Text -> Text
    replaceInvalid =
        T.map $ \c ->
            if any ($ c) [isAlphaNum, (== '-'), (== '_')] then c else ' '


-- | Slugify a 'BlogPost' tag. We don't do as much cleanup here as we do in
-- the 'slugify' function. These are more ephemeral & the generated slugs
-- never get inserted into the database.
slugifyTag :: Text -> Text
slugifyTag = T.replace " " "-" . T.toLower . T.strip
