module Examples.ThumbnailPlayer.Container where

import Prelude

import Data.Abc.Metadata (thumbnail, removeRepeatMarkers)
import Data.Abc.Parser (parse)
import Data.Abc.Midi (toMidi)
import Data.Array (index, length, mapWithIndex, unsafeIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect.Aff.Class (class MonadAff)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import VexFlow.Abc.Alignment (justifiedScoreConfig, rightJustify)
import VexFlow.Score (Renderer, clearCanvas, createScore, renderScore, initialiseCanvas, resizeCanvas)
import VexFlow.Types (Config)
import Audio.SoundFont (Instrument)
import Audio.SoundFont.Melody (Melody)
import Audio.SoundFont.Melody.Maker (toMelody_)
import Halogen.ThumbnailPlayerComponent (Query(..), Slot, component) as TNP

type State =
  {
    tuneList :: Array String
  , instruments :: Array Instrument
  , vexRenderers :: Array Renderer
  , rendered :: Boolean
  }

data Query a =
  InitializeVex a       -- Initialise Vex renderers on first reference
  | Thumbnail Int a     -- Add thumbnail for row 0 (and chain through the rest)

type Input =
  { instruments :: Array Instrument }


type ChildSlots =
  ( thumbnailPlayer :: TNP.Slot Unit )

_thumbnailPlayer = SProxy :: SProxy "thumbnailPlayer"

data Action =
    AddThumbnails       -- add all thumbnails to this page
  | PlayThumbnail Int
  | StopThumbnail

scale :: Number
scale = 0.6

canvasWidth :: Int
canvasWidth =
  500

-- The default config for each thumbnail image via Vexflow
-- This is an initial config with a small height which is
-- overridden when the image is justified to its actual dimensions
defaultThumbnailConfig :: Int -> Config
defaultThumbnailConfig index =
  { parentElementId : ("canvas" <> show index)
  , width : canvasWidth
  , height : 10       -- set to a small value so we can reduce to this between pages
  , scale : scale
  , isSVG : true      -- only use Canvas backends for debug
  }

component
   :: ∀ o m
    . MonadAff m
   => H.Component HH.HTML Query Input o m
component =
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

  initialState :: Input -> State
  initialState { instruments } =
     { tuneList : [ augustsson, fastan ]
     , instruments : instruments
     , vexRenderers : []
     , rendered : false
     }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.slot _thumbnailPlayer unit TNP.component { instruments : state.instruments } absurd
      , renderTuneList state
      , renderAddThumbnailsButton state
      , instructions state
      ]

  renderAddThumbnailsButton :: State -> H.ComponentHTML Action ChildSlots m
  renderAddThumbnailsButton state =
    if (state.rendered) then
      HH.text ""
    else
      HH.button
        [ HE.onClick \_ -> Just AddThumbnails
        , css "hoverable"
        , HP.enabled true
        ]
        [ HH.text "add thumbnails" ]

  instructions :: State -> H.ComponentHTML Action ChildSlots m
  instructions state =
    if (state.rendered) then
      HH.text "click on a thumbnail to hear it"
    else
      HH.text ""

  renderTuneList :: State -> H.ComponentHTML Action ChildSlots m
  renderTuneList state =
    HH.div
      [css "tunelist"]
      [ HH.table_ $
         (mapWithIndex tableRow state.tuneList)
      ]
    where

      tableRow :: Int -> String -> H.ComponentHTML Action ChildSlots m
      tableRow index abc =
        HH.tr
          [  ]
          [ HH.td
            []
            [ HH.text ("tune " <> show index)]
          , HH.td
            []
            [ HH.div
              [ HP.id_ ("canvas" <> show index)
              , HE.onMouseLeave \_ -> Just StopThumbnail
              , HE.onMouseDown \_ -> Just (PlayThumbnail index)
              , css "thumbnail"
              ]
              []
            ]
          ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of

    AddThumbnails -> do
      _ <- handleQuery (InitializeVex unit)
      _ <- handleQuery (Thumbnail 0 unit)
      _ <- H.modify_ (\st -> st { rendered = true } )
      pure unit

    PlayThumbnail idx -> do
      _ <- H.query _thumbnailPlayer unit $ H.tell TNP.StopMelody
      state <- H.get
      if (idx >= (length $ state.tuneList) )
        then do
          pure unit
        else do
          let
            abc = unsafePartial $ unsafeIndex state.tuneList idx
            melody = getThumbnailMelody abc
          _ <- H.query _thumbnailPlayer unit $ H.tell (TNP.PlayMelody melody)
          pure unit

    StopThumbnail -> do
      _ <- H.query _thumbnailPlayer unit $ H.tell TNP.StopMelody
      pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    -- Initialization of the Vex renderer which is done on first reference.
    -- Note, we can obly initialising after rendering the page for the first time
    -- because only then are the canvas Div elements established
    InitializeVex next -> do
      state <- H.get
      let
        foo = spy "INITIALIZEVEX" (length state.vexRenderers)
      if (length state.vexRenderers > 0)
        then do
          -- already initialized
          pure (Just next)
        else do
          let
            rows :: Array Int
            rows = [0, 1]
          renderers <- H.liftEffect $ traverse (\r -> initialiseCanvas $ defaultThumbnailConfig r) rows
          H.modify_ (\st -> st { vexRenderers = renderers } )
          pure (Just next)


    -- render the thumbnail at index idx
    Thumbnail idx next -> do
      let
        bazz =
          spy "Thumbnail Query for index: " idx
      state <- H.get
      if (idx >= (length $ state.tuneList) )
        then do
          pure (Just next)
        else do
          let
            abc = unsafePartial $ unsafeIndex state.tuneList idx
          case (Tuple (parse abc) (index state.vexRenderers idx)) of
            (Tuple (Right abcTune) (Just renderer)) -> do
              _ <- H.liftEffect $ clearCanvas renderer
              let
                foo =
                  spy "rendering thumbnail for" idx
                unjustifiedScore = createScore (defaultThumbnailConfig idx) (thumbnail abcTune)
                score = rightJustify canvasWidth scale unjustifiedScore
                config = justifiedScoreConfig score (defaultThumbnailConfig idx)
              _ <- H.liftEffect $ resizeCanvas renderer config
              _ <- H.liftEffect $ renderScore config renderer score
              handleQuery (Thumbnail (idx + 1) next)
            (Tuple (Right _) Nothing)  -> do
              let
                baz =
                  spy "no renderer for index: " idx
              handleQuery (Thumbnail (idx + 1) next)
            (Tuple (Left _) _)  -> do
              let
                baz =
                  spy "abc not parsed for index: " idx
              handleQuery (Thumbnail (idx + 1) next)


css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName

getThumbnailMelody :: String -> Melody
getThumbnailMelody abc =
  case (parse abc) of
    Right abcTune ->
      (toMelody_ 0.25 <<< toMidi <<< removeRepeatMarkers <<< thumbnail) abcTune
    _ ->
      []

augustsson :: String
augustsson =
  "X:1\r\n"
  <> "T:Engelska efter Albert Augustsson\r\n"
  <> "N:From the playing of Albert Augustsson, Bohuslän county.\r\n"
  <> "M:4/4\r\n"
  <> "R:Engelska\r\n"
  <> "S:Orust\r\n"
  <> "Z:John Watson 24/01/2015\r\n"
  <> "L:1/8\r\n"
  <> "K:A\r\n"
  <> "A>c|: e2f2 efed | c2a2 e3d | cedc BdcB | Aced cBAc |\r\n"
  <> "e2f2 efed | c2a2 e3d | cedc BdcB | A4 A>AA>B :|\r\n"
  <> "|: e2e2 e2de | f2ed B3c | d3c d2cd | e3d cdBc |\r\n"
  <> "A2a2 a2gf | e2f2 e3d | cedc BdcB |1 A4 A>AA>B :|2 [A4E4] [A4E4] |\r\n"


fastan :: String
fastan =
  "T: Fastan\r\n"
  <> "R: Polska\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 G2A2 | AF3 F8- |\r\n"
  <> "| (3:5:3F4B4cBA2 B2d2 | ge3 c4 A4- | (3:5:3A4B4cBA2 B2d2 | de3 c8- |\r\n"
  <> "| (3:5:3F4B4cBA2 B2d2 | (3:4:3g2a2f4g4 e4- | (3:c4B4A4 F2G2 | ef3 F8 |\r\n"
