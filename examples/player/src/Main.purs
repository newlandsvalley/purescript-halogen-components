module Main where

import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import Audio.SoundFont.Melody (PMelody(..))
import Data.Abc.Parser (parse)
import Data.Abc.Melody (PlayableAbc(..), defaultPlayableAbcProperties, toPlayableMelody)
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

main :: Effect Unit
main = HA.runHalogenAff do
  instruments <- H.liftAff loadInstruments
  case (parse augustsson) of 
    Right tune -> do 
      let
        props = defaultPlayableAbcProperties { tune = tune }
        melody = toPlayableMelody (PlayableAbc props)
      body <- HA.awaitBody
      _io <- runUI (component (PMelody melody) instruments) unit body
      {- if we want to change the Playable recording, we can use this:
      _ <- io.query $ H.action $ HandleNewPlayable (MidiRecording recording)
      -}
      pure unit 
    Left _ ->
      pure unit

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
