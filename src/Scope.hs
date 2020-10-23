module Scope where

import Data.Set (Set)

class Scope a where
  freeVars :: a -> Set String
  substitute :: (a -> Maybe a) -> a -> a
