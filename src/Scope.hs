{-# LANGUAGE MultiParamTypeClasses #-}

module Scope where

import Data.Set (Set)

class Scope e i where
  freeVars :: e -> Set i

class Substitute e where
  substitute :: (e -> Maybe e) -> e -> e
