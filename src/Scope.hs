module Scope where

import Data.Set (Set)
import qualified Data.Set as Set

class Scope a where
  freeVars :: a -> Set String
