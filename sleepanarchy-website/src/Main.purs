module Main (main) where

import Prelude

import App (AppEnv(..), initializeAuthStatus, runAppM)
import BaseSite as BaseSite
import Data.Either (fromRight)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (info)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Subscription (create)
import Halogen.VDom.Driver (runUI)
import Router (Route(..), router)
import Routing (match)
import Routing.PushState (makeInterface, matches)

main :: Effect Unit
main = do
  nav <- makeInterface
  initialPath <- _.pathname <$> nav.locationState
  let initialRoute = fromRight NotFound $ match router initialPath
  authStatus <- initializeAuthStatus
  pageDataSub <- create
  let env = Env { nav, authStatus, pageDataSub }
  HA.runHalogenAff do
    liftEffect $ info "Sleepanarchy.com Purescript Client Starting Up..."
    body <- HA.awaitBody
    let app = H.hoist (flip runAppM env) BaseSite.component
    driver <- runUI app initialRoute body
    liftEffect <<< void $ matches router (handlePathChange driver) nav
  where
  -- Send the 'UpdateRoute' query to the Halogen app whenever the URL changes.
  handlePathChange
    :: forall o
     . H.HalogenIO BaseSite.Query o Aff
    -> Maybe Route
    -> Route
    -> Effect Unit
  handlePathChange driver oldRoute route = do
    info $ "Path changed from " <> show oldRoute <> " to " <> show route
    launchAff_ $ void $ driver.query $ H.mkTell $ BaseSite.UpdateRoute route
