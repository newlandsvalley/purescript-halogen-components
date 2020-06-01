module Examples.Player.Main where

import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import Audio.SoundFont.Melody (Melody, PMelody(..))
import Data.Abc.Parser (parse)
import Data.Abc.Melody (toMelodyDefault)
import Effect (Effect)
import Effect.Aff (Aff)
import Data.Midi.Instrument (InstrumentName(..))
import Data.Either (Either(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.PlayerComponent (component)
import Prelude (Unit, unit, bind, pure, (<>))

loadInstruments :: Aff (Array Instrument)
loadInstruments =
  loadRemoteSoundFonts [ AcousticGrandPiano ]

{-}
note :: Int -> Int -> Number -> Number -> Number -> MidiNote
note channel id timeOffset duration gain =
  { channel : channel, id : id, timeOffset : timeOffset, duration : duration, gain : gain }

phraseSample :: Int -> Int -> Array MidiNote
phraseSample channel basePitch =
 [ note channel basePitch 0.0 0.25 1.0
 , note channel (basePitch + 2) 0.25 0.25 1.0
 , note channel (basePitch + 4) 0.5 0.5 1.0
 , note channel (basePitch + 5) 1.0 0.25 1.0
 , note channel (basePitch + 7) 1.25 0.25 1.0
 , note channel (basePitch + 11) 1.5 0.5 1.0
 ]

melodyf :: Unit -> Melody
melodyf =
  (\_ -> [ (phraseSample 0 60), (phraseSample 0 64), (phraseSample 0 67)])
-}


main :: Effect Unit
main = HA.runHalogenAff do
  instruments <- H.liftAff loadInstruments
  let
    melody = getMelody augustsson
  body <- HA.awaitBody
  -- io <- runUI (component (Just (MidiRecording recording)) instruments) (MidiRecording recording) body
  io <- runUI (component (PMelody melody) instruments) unit body
  {- if we want to change the Playable recording, we can use this:
  _ <- io.query $ H.action $ HandleNewPlayable (MidiRecording recording)
  -}
  pure unit

getMelody :: String -> Melody
getMelody abc =
  case (parse abc) of
    Right abcTune ->
      toMelodyDefault abcTune
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
