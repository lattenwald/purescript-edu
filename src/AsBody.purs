module AsBody where

import Prelude
import Data.Foreign
import Control.Monad.Eff

foreign import data DOM :: !

foreign import asBody :: forall eff. String -> Eff (dom :: DOM | eff) Unit
