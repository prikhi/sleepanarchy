module App
  ( AppM
  , runAppM
  , AppEnv(..)
  , class HasNav
  , getNav
  , class Navigation
  , newUrl
  ) where

import Prelude

import Control.Monad.Reader
  ( class MonadAsk
  , class MonadReader
  , ReaderT
  , asks
  , runReaderT
  )
import Data.Maybe (Maybe)
import Data.Traversable (for)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (unsafeToForeign)
import Router (Route, reverse)
import Routing.PushState (PushStateInterface)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent as ME

-- APP MONAD
newtype AppM a = AppM (ReaderT AppEnv Aff a)

runAppM :: forall a. AppM a -> AppEnv -> Aff a
runAppM (AppM m) = runReaderT m

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadAsk AppEnv AppM
derive newtype instance monadReaderAppM :: MonadReader AppEnv AppM

-- APP ENVIRONMENT

data AppEnv = Env { nav :: PushStateInterface }

-- NAVIGATION

class HasNav a where
  getNav :: a -> PushStateInterface

instance hasNavAppEnv :: HasNav AppEnv where
  getNav (Env e) = e.nav

class Monad m <= Navigation m where
  newUrl :: Route -> Maybe ME.MouseEvent -> m Unit

instance navigationHasNav ::
  ( HasNav env
  , MonadAsk env m
  , MonadEffect m
  ) =>
  Navigation m where
  newUrl url mbEvent = do
    void $ for mbEvent $ ME.toEvent >>> preventDefault >>> liftEffect
    nav <- asks getNav
    liftEffect $ nav.pushState (unsafeToForeign {}) $ reverse url
