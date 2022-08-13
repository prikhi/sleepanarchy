module App
  ( AppM
  , runAppM
  , AppEnv(..)
  , class HasNav
  , getNav
  , class Navigation
  , newUrl
  , class GetTime
  , getToday
  ) where

import Prelude

import Control.Monad.Reader
  ( class MonadAsk
  , class MonadReader
  , ReaderT
  , asks
  , runReaderT
  )
import Data.Date (Date)
import Data.Maybe (Maybe)
import Data.Traversable (for)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowDate)
import Foreign (unsafeToForeign)
import Router (Route, reverse)
import Routing.PushState (PushStateInterface)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent as ME

-- APP MONAD

-- | The context the Halogen app operates within.
newtype AppM a = AppM (ReaderT AppEnv Aff a)

-- | Run the given app context with the given environment.
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

-- | Runtime values used by the app.
data AppEnv = Env { nav :: PushStateInterface }

-- NAVIGATION

class HasNav a where
  getNav :: a -> PushStateInterface

instance hasNavAppEnv :: HasNav AppEnv where
  getNav (Env e) = e.nav

class Monad m <= Navigation m where
  -- | Set the page's URL to the URL for the given Route & call
  -- | `preventDefault` on the given event if present.
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

-- DATES

class Monad m <= GetTime m where
  getToday :: m Date

instance monadEffectGetTime :: MonadEffect m => GetTime m where
  getToday = liftEffect nowDate
