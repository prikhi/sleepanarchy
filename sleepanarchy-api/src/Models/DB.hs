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
import           Database.Persist.TH            ( mkMigrate
                                                , mkPersist
                                                , persistLowerCase
                                                , share
                                                , sqlSettings
                                                )
import           GHC.Generics                   ( Generic )


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User
    name Text
    password Text
    deriving Show Read Eq Ord Generic

BlogPost
    title Text
    slug Text
    description Text
    content Text
    createdAt UTCTime
    updatedAt UTCTime
    publishedAt UTCTime Maybe
    authorId UserId OnDeleteCascade
    deriving Show Read Eq Ord Generic

|]
