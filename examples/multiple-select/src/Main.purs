module Examples.MultipleSelect.Main where

import Prelude (($), (<>), Unit, bind, discard, pure, unit)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Halogen (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.Subscription as Sub
import Data.List (List(..), (:))

import Halogen.MultipleSelectComponent (Context, Message(..), State, component) as MSC

main :: Effect Unit
main = HA.runHalogenAff do
  let
    initialState :: ∀ i. i -> MSC.State
    initialState _ =
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
  io <- runUI (MSC.component ctx initialState) unit body
  
  _ <- liftEffect $ Sub.subscribe io.messages \(MSC.CommittedSelections instruments) -> do
    liftEffect $ log $ "Selected instruments: " <> intercalate ", " instruments
    pure Nothing

  pure unit
