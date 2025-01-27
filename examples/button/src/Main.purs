module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Halogen.SimpleButtonComponent (toggledLabelComponent, Message(..)) as B
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen (liftEffect)
import Halogen.Subscription as HS
import Data.Maybe (Maybe(..))

main :: Effect Unit
main = HA.runHalogenAff
  do
    body <- HA.awaitBody
    io <- runUI (B.toggledLabelComponent "start" "stop" ) unit body
    
    liftEffect $ HS.subscribe io.messages \msg -> do
      case msg of 
        B.Toggled bool -> do
          liftEffect $ log $ "button pressed on: " <> (show bool)
          pure Nothing

