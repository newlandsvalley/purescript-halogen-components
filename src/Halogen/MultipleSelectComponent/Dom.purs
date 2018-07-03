module Halogen.MultipleSelectComponent.Dom
  ( resetDefaultSelected ) where

import Prelude (Unit)
import Effect (Effect)

-- | We use FFI into javascript just in order to reset the selected option
-- | in the select menu to the default value whenever something else
-- | has been chosen
foreign import resetDefaultSelected :: Effect Unit
