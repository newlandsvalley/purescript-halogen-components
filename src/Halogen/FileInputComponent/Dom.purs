module Halogen.FileInputComponent.Dom
  ( resetInputValue ) where

import Prelude (Unit)
import Effect (Effect)

-- | We use FFI into javascript just in order to reset the selected file input
-- | value to "" which should allow onChange to work for repeated files of
-- | the same name
foreign import resetInputValue :: String -> Effect Unit
