module Halogen.SimpleButtonComponent where

-- | a trivially simple button where the toggled variant has a button with text
-- | that toggles each time it is pressed.
-- | The plain vanilla component has static text.
-- | Whether or not it is enabled may be set externally via a query
-- | As from Halogen 5, toggling is entirely self-contained within the
-- | button and is now represented as an Action

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { isOn :: Boolean
  , isEnabled :: Boolean
  }

data Action = Toggle

data Query a =
  UpdateEnabled Boolean a

data Output = Toggled Boolean

-- | the basic component has a label on the button that never alters
component :: forall i m. String -> H.Component HH.HTML Query i Output m
component label =
  toggledLabelComponent label label

-- | but the toggled label component toggles between labels each time the button is pressed
toggledLabelComponent :: forall i m. String -> String -> H.Component HH.HTML Query i Output m
toggledLabelComponent offLabel onLabel =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Nothing
        , finalize = Nothing
        }
    }
  where

  initialState :: i -> State
  initialState _ =
    { isOn : false
    , isEnabled : true
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    let
      label =
        if (state.isOn) then
          onLabel
        else
          offLabel
    in
      HH.button
        [ HE.onClick \_ -> Just Toggle
        , HP.class_ $ ClassName "hoverable"
        , HP.enabled state.isEnabled
        ]
        [ HH.text label ]

  handleQuery :: forall o a. Query a -> H.HalogenM State Action () o m (Maybe a)
  handleQuery = case _ of
    UpdateEnabled isEnabled next -> do
      _ <- H.modify (\state -> state {isEnabled = isEnabled})
      pure (Just next)

  handleAction ∷ Action → H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Toggle -> do
      state <- H.get
      let nextState = state { isOn = not state.isOn }
      H.put nextState
      H.raise $ Toggled nextState.isOn
