module Halogen.ThumbnailPlayerComponent
  ( Query(..)
  , Slot
  , component) where

-- | A Halogen component which is intended to act as a player for melodies
-- | produced from ABC thumbnails which are necessarily short but which
-- | nevertheless need stopping (and hence interrupting).
-- | There is no UI - the player is intended to be started or stopped by
-- | the parent component.

import Prelude

import Audio.SoundFont (Instrument, playNotes)
import Audio.SoundFont.Melody (Melody, MidiPhrase)
import Control.Monad.State.Class (class MonadState)
import Data.Array (null, index)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH

type Slot = H.Slot Query Void

type Input =
  { instruments :: Array Instrument }

-- actions are those that usually derive from HTML events but we have no HTML!
-- only used here to enamle receive and finalize.
data Action =
    StopMelodyAction                 -- invoke stop
  | HandleInput Input                -- obtain a new melody to play

data Query a =
    PlayMelody Melody a              -- play
  | StepMelody a                     -- step to the next phrase
  | StopMelody a                     -- stop and reset to zero
  | IsPlaying (Boolean -> a)         -- is the player playing

type State =
  { instruments :: Array Instrument  -- the instrument soundfonts available
  , melody :: Melody                 -- the melody to play
  , playing :: Boolean               -- is the player playing?
  , phraseIndex :: Int               -- the current phrase being played
  , phraseLength :: Number           -- the duration of the phrase currently playing
  }

component :: ∀ o m.
  MonadAff m =>
  H.Component Query Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< HandleInput
        , initialize = Nothing
        , finalize = Just StopMelodyAction
        }
    }
  where

  initialState :: Input -> State
  initialState input =
    { instruments : input.instruments
    , melody : []
    , playing : false
    , phraseIndex : 0
    , phraseLength : 0.0
    }

  -- we are renderless
  render :: State -> H.ComponentHTML Action () m
  render _ =
    HH.text ""

handleQuery :: forall a o m.
  MonadAff m =>
  Query a ->
  H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of

  PlayMelody melody next -> do
    -- stop any previously playing melody
    _ <- stop

    if (not (null melody))
      then do
        -- play
        _ <- H.modify (\st -> st { phraseIndex = 0, playing = true, melody = melody })
        _ <- handleQuery (StepMelody unit)
        pure (Just next)
      else do
        pure (Just next)

  -- StepMelody plays the current phrase and then steps the pointer to the next one
  -- it must respect interruption from the parent between steps
  StepMelody next -> do
    state <- H.get

    if (state.playing && (not (null state.melody)))
      then do
        -- play
        nextInstruction <- step
        handleQuery (nextInstruction next)
      else do
        pure (Just next)

  StopMelody next -> do
    _ <- stop
    pure (Just next)

  IsPlaying f -> do
    isPlaying <- H.gets _.playing
    pure (Just (f isPlaying))

-- handling an action just delegates to the appropriate query
handleAction ∷ ∀ o m.
  MonadAff m =>
  Action →
  H.HalogenM State Action () o m Unit
handleAction = case _ of

  -- just here so we can invoke it from the finalizer
  StopMelodyAction -> do
    _ <- stop
    pure unit

  -- stop then handle a new input from the parent component
  HandleInput input  -> do
    newState <- stop
    H.put newState { instruments = input.instruments }
    pure unit

-- stop the playback
stop :: ∀ m.
  Bind m =>
  MonadState State m =>
  MonadAff m =>
  m State
stop =
  H.modify (\st -> st  { phraseIndex = 0, playing = false, melody = [] })

-- step to the next part of the melody if we're still running
step :: forall m a.
    Bind m =>
    MonadState State m =>
    MonadEffect m =>
    MonadAff m =>
    m ( a -> Query a)
step = do
  state <- H.get
  let
    mPhrase = locateNextPhrase state
  case mPhrase of
    Just (midiPhrase) -> do
      -- play the phrase
      -- only NoteOn events produce sound
      phraseLength <- H.liftEffect (playNotes state.instruments midiPhrase)
      -- step the index and put it into the state
      let
        newState =
          state { phraseIndex = state.phraseIndex + 1
                , phraseLength = phraseLength
                }
      H.put newState
      -- delay whilst we're playing the phrase
      _ <-  H.liftAff $ delay (Milliseconds ((phraseLength) * 1000.0))
      pure StepMelody
    _ ->
      pure StopMelody

-- | locate the next MIDI phrase from the performance
locateNextPhrase :: State -> Maybe MidiPhrase
locateNextPhrase state =
  if (not state.playing) || (null state.melody) then
    Nothing
  else
    index state.melody (state.phraseIndex)
