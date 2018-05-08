module Main where

import Audio.SoundFont (AUDIO, Instrument, loadRemoteSoundFonts)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Data.HTTP.Method (Method(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Data.MediaType (MediaType(..))
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Midi.Instrument (InstrumentName(..))
import Data.Midi.Parser (normalise, parse)
import Data.Either (Either(..), fromRight)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint8Array, toIntArray)
import Data.Int.Bits (and)
import Data.Char (fromCharCode)
import Data.String (fromCharArray)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.PlayerComponent (component, Query(..))
import Audio.SoundFont.Melody.Class (MidiRecording(..))
import Prelude (Unit, unit, bind, map, pure, (<>), ($), (<<<))
import Partial.Unsafe (unsafePartial)

loadInstruments :: ∀ eff .
  Aff ( ajax :: AJAX, au ::AUDIO | eff)
    (Array Instrument)
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

loadMidi :: ∀ e.
  String
  -> Aff
     ( ajax :: AJAX
     | e
     )
     ArrayBuffer
loadMidi name = do
  let
    url =
      "midi/" <> name
  res <- affjax $ defaultRequest
           { url = url
           , method = Left GET
           , headers = [ Accept (MediaType "audio/midi")]
           }
  pure $ res.response

toUint8Array :: ArrayBuffer ->  Uint8Array
toUint8Array ab =
  asUint8Array $ whole ab

-- | convert the unsigned integer array to the 'binary string' which is expected
-- | by the MIDI parser.  This is the same format that would be returned either
-- | by readAsBinaryString or by using the override MIME type hack in XmlHttpRequest.
-- | denormalise is the 'mirror' function to the MIDI parser's normalise function.
-- | However masking off all but the lowest byte is not strictly necessary because
-- | normalise will also do that.
denormalise :: Uint8Array -> String
denormalise =
  let
    f = fromCharCode <<< ((and) 0xFF)
  in
    fromCharArray <<< map f <<< toIntArray

main :: Eff (HA.HalogenEffects (ajax :: AJAX, au :: AUDIO)) Unit
main = HA.runHalogenAff do
  instruments <- H.liftAff loadInstruments
  midiBytes <- H.liftAff $ loadMidi "lillasystern.midi"
  let
    erecording = (parse <<< normalise <<< denormalise <<< toUint8Array) midiBytes
    recording = unsafePartial $ fromRight erecording
  body <- HA.awaitBody
  -- io <- runUI (component (Just (MidiRecording recording)) instruments) (MidiRecording recording) body
  io <- runUI (component (MidiRecording recording) instruments) (MidiRecording recording) body
  {- if we want to change the Playable recording, we can use this:
  _ <- io.query $ H.action $ HandleNewPlayable (MidiRecording recording)
  -}
  pure unit
