{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Models.DB where

import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Database.Persist               ( ToBackendKey(..) )
import           Database.Persist.TH            ( mkEntityDefList
                                                , mkMigrate
                                                , mkPersist
                                                , persistLowerCase
                                                , share
                                                , sqlSettings
                                                )
import           GHC.Generics                   ( Generic )
import           Servant.Auth.Server            ( FromJWT
                                                , ToJWT
                                                )
import           Servant.Docs                   ( ToSample(..)
                                                , singleSample
                                                )


share [mkPersist sqlSettings, mkEntityDefList "tableDefs", mkMigrate "migrateAll"] [persistLowerCase|

User
    name Text
    password Text
    UniqueUser name
    deriving Show Read Eq Ord Generic

BlogPost
    title Text
    slug Text
    description Text
    content Text
    -- TODO: ditch this for custom PostgresTextArray type?
    -- Lets us process once on save instead of every fetch
    tags Text
    authorId UserId OnDeleteCascade
    categoryId BlogCategoryId OnDeleteRestrict
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    publishedAt UTCTime Maybe
    UniqueBlogPost slug
    deriving Show Read Eq Ord Generic

BlogCategory
    title Text
    slug Text
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniqueBlogCategory slug
    deriving Show Read Eq Ord Generic

|]


instance ToJWT UserId
instance FromJWT UserId

instance ToSample BlogPostId where
    toSamples _ = singleSample $ fromBackendKey 9001
