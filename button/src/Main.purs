module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Halogen.SimpleButtonComponent (component, Message(..)) as B
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Control.Coroutine as CR
import Data.Maybe (Maybe(..))

main :: Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
main = HA.runHalogenAff
  do
    body <- HA.awaitBody
    io <- runUI (B.component "press me" ) unit body

    io.subscribe $ CR.consumer \(B.Toggled bool) -> do
      log $ "button pressed on: " <> (show bool)
      pure Nothing
