module Examples.Button.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Halogen.SimpleButtonComponent (toggledLabelComponent, Output(..)) as B
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen (liftEffect)
import Control.Coroutine as CR
import Data.Maybe (Maybe(..))

main :: Effect Unit
main = HA.runHalogenAff
  do
    body <- HA.awaitBody
    io <- runUI (B.toggledLabelComponent "start" "stop" ) unit body

    io.subscribe $ CR.consumer \(B.Toggled bool) -> do
      liftEffect $ log $ "button pressed on: " <> (show bool)
      pure Nothing

    pure unit
