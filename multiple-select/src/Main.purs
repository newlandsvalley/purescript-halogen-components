module Main where

import Prelude (Unit, bind, unit)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Data.List (List(..), (:))

import Halogen.MultipleSelectComponent (Context, State, component) as MSC
import Halogen.MultipleSelectComponent.Dom

main :: Effect Unit
main = HA.runHalogenAff do
  let
    initialState :: MSC.State
    initialState =
      { available : ("piano" : "guitar" : "mandolin" : "bouzouki" : Nil)
      , selected : Nil
      }
    ctx :: MSC.Context
    ctx =
        { selectPrompt : "select instruments"
        , commitPrompt : "change instruments:"
        , commitButtonText : "load"
        }
  body <- HA.awaitBody
  runUI (MSC.component ctx initialState) unit body
