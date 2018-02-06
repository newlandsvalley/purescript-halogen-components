module Main where

import Prelude (Unit, bind, unit)
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Data.List (List(..), (:))

import MultipleSelectComponent (component) as MS
import MultipleSelect.Dom (SDOM)

main :: Eff (HA.HalogenEffects (sdom :: SDOM)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (MS.component "choose instruments" ("piano" : "guitar" : "mandolin" : "bouzouki" : Nil) ) unit body
