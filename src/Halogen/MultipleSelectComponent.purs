module Halogen.MultipleSelectComponent where

import Prelude

import Data.Maybe (Maybe(..))
import Data.List (List(..), elem, filter, reverse, toUnfoldable, (:))
import Data.Array (cons) as A
import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..), HTML)
import Halogen.MultipleSelectComponent.Dom (resetDefaultSelected)

type Slot = H.Slot Query Message

data Action =
    AddSelection String
  | RemoveSelection String
  | CommitSelections

data Query a =
    ClearSelections a
  | GetSelections (List String -> a)

data Message = CommittedSelections (List String)

type Context = {
    selectPrompt       :: String   -- the user instruction on what to select
  , commitPrompt       :: String   -- the user prompt for committing changes
  , commitButtonText   :: String   -- the text on the commit button
  }

type State = {
    available :: List String       -- available options
  , selected  :: List String       -- currently selected options
  }

component :: ∀ i m. MonadAff m => Context -> (i -> State) -> H.Component HH.HTML Query i Message m
component ctx initialState =
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

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.class_ $ ClassName "msSelectDiv" ]
      [ addSelectionDropdown state
      , viewSelections state
      , commitSelectionsButton state
      ]

  -- allow the user to add a selection to the growing multi-select list
  addSelectionDropdown :: State -> H.ComponentHTML Action () m
  addSelectionDropdown state =
    let
      f :: ∀ p j. String -> HTML p j
      f s =
          HH.option
            [ HP.disabled (elem s state.selected) ]
            [ HH.text s]
    in
      HH.div
        [ HP.class_ $ ClassName "msAddSelectionDiv" ]
        [
          HH.select
            [ HP.class_ $ ClassName "msAddSelection"
            , HP.id_  "selection-menu"
            , HP.value ctx.selectPrompt
            , HE.onValueChange  (Just <<< AddSelection)
            ]
            (A.cons
              (HH.option [ HP.disabled true ] [ HH.text ctx.selectPrompt])
              (map f $ toUnfoldable state.available)
            )
        ]

  commitSelectionsButton :: State -> H.ComponentHTML Action () m
  commitSelectionsButton state =
    case state.selected of
      Nil ->
        HH.div_ []
      _ ->
        HH.div
          [ HP.class_ (H.ClassName "msCommitDiv") ]
          [ HH.label
             [ HP.class_ (H.ClassName "msCommitLabel") ]
             [ HH.text ctx.commitPrompt ]
          , HH.button
             [ HP.class_ $ ClassName "msCommit hoverable"
             , HE.onClick (\_ -> Just CommitSelections)
             ]
             [ HH.text ctx.commitButtonText ]
          ]

  -- list the currently selected options
  viewSelections :: State -> H.ComponentHTML Action () m
  viewSelections state =
    let
      -- f :: ∀ p i. String -> HTML p i
      f s =
        HH.li
          [ HP.class_ $ ClassName "msListItem" ]
          [ HH.span
              [ HP.class_ $ ClassName  "msListItemLabel" ]
              [ HH.text s]
          , HH.a
              [ HP.class_ $ ClassName  "msListItemRemove"
              , HE.onClick (\_ -> Just $ RemoveSelection s)
              ]
              [ HH.text " remove"]
          ]
    in
      HH.div_
        (map f $ toUnfoldable state.selected)

handleQuery :: forall o m a. Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  ClearSelections next -> do
    _ <- H.modify (\state -> state { selected = Nil })
    pure (Just next)
  GetSelections reply -> do
    state <- H.get
    pure (Just $ reply state.selected)


handleAction ∷ ∀ m. MonadAff m => Action → H.HalogenM State Action () Message m Unit
handleAction = case _ of
  AddSelection s -> do
    _ <- H.modify (\state -> state { selected = addSelection s state.selected })
    H.liftEffect resetDefaultSelected
  RemoveSelection s  -> do
    _ <- H.modify (\state -> state { selected = removeSelection s state.selected })
    pure unit
  CommitSelections -> do
    state <- H.get
    let
      selected = state.selected
    _ <- H.modify (\st -> st { selected = Nil })
    H.raise $ CommittedSelections selected


-- add a selection to the end of the list
addSelection :: String -> List String -> List String
addSelection s ss =
  reverse $ s : (reverse ss)

-- remove a selection from the list
removeSelection :: String -> List String -> List String
removeSelection s ss =
  filter ((/=) s) ss
