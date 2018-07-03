module Halogen.PlayerComponent where

-- | A Halogen component which acts as a generic player for any music
-- | source that can be converted into a Melody - in other words
-- | one that is an instance of Playable

-- | When looking at the data structures, whenever we have a type variable p
-- | then look at it as if it were defined as ∀ p. Playable p => p
-- | It doesn't seem possible actually to define a type like this and so the
-- | Query and State data types have been parameterised with (simply) p
-- | but all functions that involve these types require the Playable constraint.



import Prelude

import Audio.SoundFont (Instrument, playNotes, instrumentChannels)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (class MonadAff)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Control.Monad.State.Class (class MonadState)
import Data.Array (null, index, length)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (PropName(..))
import Halogen.PlayerComponent.Style (capsuleStyle, playerBlockStyle, playerStyle
  , buttonStyle)
import Audio.SoundFont.Melody.Class (class Playable, toMelody)
import Audio.SoundFont.Melody (Melody, MidiPhrase)


-- | now we have tri-state logic for playback state because of the pending status
data PlaybackState =
    PLAYING           -- the melody is playing
  | PENDINGPAUSED     -- the pause button has been hit, but the phrase is stil finishing
  | PAUSED            -- the melody is not playing  (paused or stopped)

derive instance genericPlaybackState :: Generic PlaybackState _
instance showEvent :: Show PlaybackState where
  show = genericShow
instance eqEvent :: Eq PlaybackState where
  eq = genericEq


data Message = IsPlaying Boolean

data Query p a =
    SetInstruments (Array Instrument) a
  | PlayMelody PlaybackState a             -- play | pause
  | StepMelody a                           -- step to the next phrase
  | StopMelody a                           -- stop and set index to zero
  | EnablePlayButton a                     -- re-enable the play button
  | HandleNewPlayable p a                  -- obtain a new melody to play


type State p =
  { instruments :: Array Instrument  -- the instrument soundfonts available
  , melody :: Melody                 -- the melody to play
  , playing :: PlaybackState         -- the state of the playback
  , phraseIndex :: Int               -- the current phrase being played
  , phraseLength :: Number           -- the duration of the phrase currently playing
  , playable :: p                    -- the playable piece of music to convert to a melody
  }

-- | In this branch, there is no receiver function or receiver input
component :: ∀ p. Playable p => p -> Array Instrument -> H.Component HH.HTML (Query p) Unit Message Aff
component playable instruments =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  -- | the initial state of the player
  -- | We can choose to construct it with a Ployable and/or defer to
  -- | the receiver function later on to get hold of it
  initialState :: State p
  initialState =
    { instruments : instruments
    , melody : []
    , playing : PAUSED
    , phraseIndex : 0
    , phraseLength : 0.0
    , playable : playable
    }


  render :: Playable p => State p -> H.ComponentHTML (Query p)
  render state =
    let
      sliderPos =
        toNumber $ state.phraseIndex
      capsuleMax =
        toNumber $ length state.melody
      startImg =
        "assets/images/play.png"
      stopImg =
        "assets/images/stop.png"
      pauseImg =
        "assets/images/pause.png"
      -- the action toggles the PLAYING - PAUSED status
      playAction =
        if (state.playing == PLAYING) then
           PlayMelody PAUSED
        else
           PlayMelody PLAYING
      playButtonImg =
        if (state.playing == PAUSED) then
          startImg
        else
          -- the pause image is displayed both if the tune is to be PlayMelody
          -- or else if it is disabled pending a pause
          pauseImg
      -- the buttons are temporarily disabled after a pause command
      isDisabled =
        (state.playing == PENDINGPAUSED)
    in
      HH.div [ playerBlockStyle ]
        [ HH.div [ playerStyle ]
          [

                 -- , playerStyle
            HH.input
              [ HP.type_ HP.InputImage
              , HP.disabled isDisabled
              , HP.src playButtonImg
              , HE.onClick (HE.input_ playAction)
              , buttonStyle
              ]
          ,  HH.input
              [ HP.type_ HP.InputImage
              , HP.disabled isDisabled
              , HP.src stopImg
              , HE.onClick (HE.input_ StopMelody)
              , buttonStyle
              ]
          , HH.progress
                [ HP.max capsuleMax
                , progressValue sliderPos
                , capsuleStyle
                ] []
          ]
          {- debug
          , HH.div_
            [ HH.text ("melody length: " <> show (length state.melody))
            , HH.text ("no of instruments: " <> show (length state.instruments))]
          -}
        ]

  eval :: Playable p => (Query p) ~> H.ComponentDSL (State p) (Query p) Message Aff
  eval = case _ of

    -- when we change the instruments (possibly im mid-melody) we need to
    -- re-initialise and remove the old melody which will need to be
    -- recomputed with the new instruments (done when play is first pressed)
    SetInstruments instruments' next -> do
      state <- H.get
      -- set new state to PENDINGAUSED if we interrupt mid-tune
      let
        newPlayingState =
          if (state.playing == PLAYING)
            then PENDINGPAUSED
          else
            PAUSED
      _ <- H.modify (\st -> st { instruments = instruments'
                               , phraseIndex = 0
                               , phraseLength = 0.0
                               , playing = newPlayingState
                               , melody = []
                               })
      pure next


    -- PlayMelody responds to the toggled button PLAY/PAUSE
    -- however it also establishes the melody on first reference
    PlayMelody button next -> do
      state <- H.get

      when (null state.melody) do
        establishMelody

      state' <- H.get

      if ((button == PLAYING) && (not (null state'.melody)))
        then do
          -- play
          H.raise $ IsPlaying true
          _ <- H.modify (\st -> st { playing = PLAYING})
          eval (StepMelody next)
        else do
          -- pause
          nextInstruction <- temporarilyFreezePlayButton
          eval (nextInstruction next)

    -- StepMelody plays the current phrase and then steps the pointer to the next one
    -- it must respect any button presses in between steps
    StepMelody next -> do
      state <- H.get

      if ((state.playing == PLAYING) && (not (null state.melody)))
        then do
          -- play
          nextInstruction <- step
          eval (nextInstruction next)
        else do
          -- pause
          nextInstruction <- temporarilyFreezePlayButton
          eval (nextInstruction next)

    -- EnablePlayButton unfreezes the play button which is frozen after being
    -- pressed so as to avoid playing the melody twice simultaneously
    EnablePlayButton next -> do
      H.raise $ IsPlaying false
      _ <- H.modify (\state -> state { playing = PAUSED})
      pure next

    -- StopMelody resets the melody index back to the start
    StopMelody next -> do
      newState <- stop
      H.put newState
      pure next

    -- stop then handle a new melody when requested externally
    HandleNewPlayable playable' next -> do
      state <- H.get
      newState <- stop
      H.put newState { playable = playable', melody = [] }
      pure next


-- establish the melody by conversio from the playable
establishMelody :: ∀ m p.
  Bind m =>
  Playable p =>
  MonadState (State p) m =>
  MonadAff m =>
  m Unit
establishMelody = do
  state <- H.get
  let
    melody =
      toMelody state.playable (instrumentChannels state.instruments)
  _ <- H.modify (\st -> st { melody = melody})
  pure unit

-- stop the playback
stop :: ∀ m p.
  Bind m =>
  Playable p =>
  MonadState (State p) m =>
  MonadAff m =>
  m (State p)
stop = do
  state <- H.get
  if (state.playing == PLAYING)
    then do
      _ <- temporarilyFreezePlayButton
      state' <- H.get
      pure $ state' { phraseIndex = 0, playing = PAUSED}
    else do
      pure $ state { phraseIndex = 0, playing = PAUSED}

-- step to the next part of the melody

step :: forall m a p.
    Bind m =>
    Playable p =>
    MonadState (State p) m =>
    MonadEffect m =>
    MonadAff m =>
    m ( a -> Query p a)
step = do
  state <- H.get
  let
    mPhrase = locateNextPhrase state
  case mPhrase of
    Just (midiPhrase) -> do
      -- play the phrase
      phraseLength <- H.liftEffect (playEvent state.instruments midiPhrase)
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

-- temporarily freeze the play button so that the melody is allowed
-- (asynchronously) to end before it's re-enabled
temporarilyFreezePlayButton :: ∀ m a p.
  Bind m =>
  Playable p =>
  MonadState (State p) m =>
  MonadAff m =>
  m (a -> Query p a)
temporarilyFreezePlayButton = do
  state <- H.get
  H.put $ state { playing = PENDINGPAUSED }
  let
    msDelay = state.phraseLength
  _ <-  H.liftAff $ delay (Milliseconds (msDelay * 1000.0))
  pure EnablePlayButton

-- | locate the next MIDI phrase from the performance
locateNextPhrase :: ∀ p. Playable p => State p -> Maybe MidiPhrase
locateNextPhrase state =
  if (not (state.playing == PLAYING)) || (null state.melody) then
    Nothing
  else
    index state.melody (state.phraseIndex)

-- | play a MIDI Phrase (a bunch of MIDI notes)
-- | only NoteOn events produce sound
playEvent :: Array Instrument -> MidiPhrase -> Effect Number
playEvent instruments midiPhrase =
  playNotes instruments midiPhrase

-- halogen bug workaround
progressValue :: forall r i. Number -> HP.IProp (value :: Number | r) i
progressValue = HP.prop (PropName "value")
