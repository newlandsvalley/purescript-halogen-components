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
import Audio.SoundFont.Melody (Melody, MidiPhrase)
import Audio.SoundFont.Melody.Class (class Playable, toMelody)
import Control.Monad.State.Class (class MonadState)
import Data.Array (null, index, length)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (PropName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.PlayerComponent.Style (capsule, capsuleStyle, playerBlockStyle, playerStyle, buttonStyle)
import Web.UIEvent.MouseEvent (clientX)
import Web.HTML.HTMLElement (HTMLElement, getBoundingClientRect)

type Slot p = H.Slot (Query p) Message

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

-- actions are those that derive from HTML events
data Action =
    PlayMelodyAction PlaybackState           -- invoke play | pause
  | StopMelodyAction                         -- invoke stop
  | CapsuleMouseButton Boolean               -- Mouse down over the player capsule
  | CapsuleDragPosition Number               -- the position where the capsule is dragged to

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
  , capsuleMouseDown :: Boolean      -- the state of any mouse button over the player capsule
  , capsuleDragPosition :: Number    -- temp
  }

-- | In this branch, there is no receiver function or receiver input
component :: ∀ p m.
  Playable p =>
  MonadAff m =>
  p ->
  Array Instrument ->
  H.Component (Query p) Unit Message m
component playable instruments =
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

  -- | the initial state of the player
  -- | We can choose to construct it with a Playable and/or defer to
  -- | the receiver function later on to get hold of it
  initialState :: ∀ i. i -> State p
  initialState _ =
    { instruments : instruments
    , melody : []
    , playing : PAUSED
    , phraseIndex : 0
    , phraseLength : 0.0
    , playable : playable
    , capsuleMouseDown : false
    , capsuleDragPosition : 0.0
    }


  render :: State p -> H.ComponentHTML Action () m
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
           PlayMelodyAction PAUSED
        else
           PlayMelodyAction PLAYING
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
      HH.div
        [ playerBlockStyle ]
        [ HH.div
          [ playerStyle ]
          [

            -- , playerStyle
            HH.input
              [ HP.type_ HP.InputImage
              , HP.disabled isDisabled
              , HP.src playButtonImg
              , HE.onClick (\_ -> playAction)
              --, HE.onClick (HE.input_ playAction)
              , buttonStyle
              ]
          ,  HH.input
              [ HP.type_ HP.InputImage
              , HP.disabled isDisabled
              , HP.src stopImg
              , HE.onClick \_ -> StopMelodyAction
              --, HE.onClick (\_ -> EvalQuery StopMelody)
              , buttonStyle
              ]
          , HH.progress
                [ HP.max capsuleMax
                , HP.ref $ H.RefLabel capsule.ref
                , progressValue sliderPos
                , HE.onMouseDown \_ -> CapsuleMouseButton true
                , HE.onMouseUp \_ -> CapsuleMouseButton false
                , HE.onMouseLeave \_ -> CapsuleMouseButton false
                , HE.onMouseMove \e -> CapsuleDragPosition ((toNumber <<< clientX) e)
                , capsuleStyle
                ] []
          ]
          {- debug
          , HH.div_
            [ HH.text ("Capsule Mouse down? " <> show (state.capsuleMouseDown))
            , HH.text ("Capsule drag position " <> show (state.capsuleDragPosition))
            , HH.text ("melody length: " <> show (length state.melody))
            , HH.text ("no of instruments: " <> show (length state.instruments))
            ]
          -}
        ]


handleQuery :: forall a m p.
  MonadAff m =>
  Playable p =>
  Query p a ->
  H.HalogenM (State p) Action () Message m (Maybe a)
handleQuery = case _ of

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
      pure (Just next)

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
        handleQuery (StepMelody next)
      else do
        -- pause
        nextInstruction <- temporarilyFreezePlayButton
        handleQuery (nextInstruction next)

  -- StepMelody plays the current phrase and then steps the pointer to the next one
  -- it must respect any button presses in between steps
  StepMelody next -> do
    state <- H.get

    if ((state.playing == PLAYING) && (not (null state.melody)))
      then do
        -- play
        nextInstruction <- step
        handleQuery (nextInstruction next)
      else do
        -- pause
        nextInstruction <- temporarilyFreezePlayButton
        handleQuery (nextInstruction next)

  -- EnablePlayButton unfreezes the play button which is frozen after being
  -- pressed so as to avoid playing the melody twice simultaneously
  EnablePlayButton next -> do
    H.raise $ IsPlaying false
    _ <- H.modify (\state -> state { playing = PAUSED})
    pure (Just next)

  -- StopMelody resets the melody index back to the start
  StopMelody next -> do
    H.raise $ IsPlaying false
    newState <- stop
    H.put newState
    pure (Just next)

  -- stop then handle a new melody when requested externally
  HandleNewPlayable playable' next -> do
    newState <- stop
    H.put newState { playable = playable', melody = [] }
    pure (Just next)

-- handling an action from HTML events just delegates to the appropriate query
-- I'm not sure why using unit is kosher for  the query's a param here but it
-- seems OK.
handleAction ∷ ∀ m p.
  MonadAff m =>
  Playable p =>
  Action →
  H.HalogenM (State p) Action () Message m Unit
handleAction = case _ of
  StopMelodyAction -> do
    _ <- handleQuery (StopMelody unit)
    pure unit
  PlayMelodyAction playbackState -> do
    _ <- handleQuery (PlayMelody playbackState unit)
    pure unit
  CapsuleMouseButton isDown -> do
    _ <- H.modify (\state -> state { capsuleMouseDown = isDown})
    pure unit
  CapsuleDragPosition viewportPos -> do
    state <- H.get
    mCapsuleElement <- H.getHTMLElementRef (H.RefLabel capsule.ref)
    -- get the left hand side of the capsule in the viewport
    capsuleXPos <- maybe (pure 0.0) (H.liftEffect <<< getCapsuleX) mCapsuleElement
    when state.capsuleMouseDown do
      let
        capsuleDragPosition = viewportPos - capsuleXPos
        newIndex = floor (capsuleDragPosition / capsule.width * toNumber (length state.melody))
      H.modify_ (\st -> st { capsuleDragPosition = capsuleDragPosition, phraseIndex = newIndex})
    pure unit

-- establish the melody by conversion from the playable
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

-- get the X position of the player capsule
getCapsuleX :: HTMLElement -> Effect Number
getCapsuleX capsuleElement = do
  rect <- getBoundingClientRect capsuleElement
  pure $ rect.left
