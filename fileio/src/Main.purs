module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Halogen.FileInputComponent (component, Message(..)) as FI
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Control.Coroutine as CR
import JS.FileIO (FILEIO)
import Data.MediaType (MediaType(..))
import Data.Maybe (Maybe(..))

main :: Eff (HA.HalogenEffects (fileio :: FILEIO, console :: CONSOLE)) Unit
main = HA.runHalogenAff
  let
    ctx = { componentId : "midiinput"
          , isBinary : true
          , prompt : "load a midi file"
          , accept : MediaType ".midi, .mid"
          }
    {-}
    ctx = { componentId : "abcinput"
          , isBinary : false
          , prompt : "load an abc file:"
          , accept : MediaType ".abc"
          }
    -}
  in do
    body <- HA.awaitBody
    io <- runUI (FI.component ctx ) unit body

    io.subscribe $ CR.consumer \(FI.FileLoaded filespec) -> do
      log $ "File was loaded: " <> filespec.name
      pure Nothing
