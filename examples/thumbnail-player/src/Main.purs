module Main where

import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import Effect (Effect)
import Effect.Aff (Aff)
import Data.Midi.Instrument (InstrumentName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Container (component)
import Prelude (Unit, unit, bind, pure)


loadInstruments :: Aff (Array Instrument)
loadInstruments =
  loadRemoteSoundFonts [ AcousticGrandPiano ]

main :: Effect Unit
main = HA.runHalogenAff do
  instruments <- H.liftAff loadInstruments
  body <- HA.awaitBody
  _io <- runUI component { instruments : instruments } body
  pure unit
