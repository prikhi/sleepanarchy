module Main (main) where

import Prelude

import BaseSite as BaseSite
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (info)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Router (Route, router)
import Routing.PushState (makeInterface, matches)

main :: Effect Unit
main = do
  nav <- makeInterface
  HA.runHalogenAff do
    liftEffect $ info "Sleepanarchy.com Purescript Client Starting Up..."
    body <- HA.awaitBody
    driver <- runUI BaseSite.component unit body
    liftEffect <<< void $ matches router (handlePathChange driver) nav
  where
  handlePathChange
    :: forall o
     . H.HalogenIO BaseSite.Query o Aff
    -> Maybe Route
    -> Route
    -> Effect Unit
  handlePathChange driver oldRoute route = do
    info $ "Path changed from (" <> show oldRoute <> ") to (" <> show route <>
      ")"
    launchAff_ $ void $ driver.query $ H.mkTell $ BaseSite.UpdateRoute route
