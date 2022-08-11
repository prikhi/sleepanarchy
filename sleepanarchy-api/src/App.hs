{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App where

import           Control.Monad.Reader
import           Servant.Server                 ( Handler )


data Config = Config {}

newtype App a = App { runApp :: ReaderT Config Handler a } deriving (Functor, Applicative, Monad, MonadReader Config)
