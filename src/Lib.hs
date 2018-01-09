{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State
import LLVM.AST hiding (Module, VoidType)

data Expr = Nat Int
          | Plus Expr Expr
          | Minus Expr Expr
          | Ref String
          | Let String Expr Expr
          deriving (Eq, Show)

example :: Expr
example = Let "x" (Plus (Nat 1) (Nat 2)) $ Minus (Nat 5) (Ref "x")


class MonadFresh m where
  uniqueName :: String -> m String

fresh :: MonadFresh m => m String
fresh = uniqueName ""

instance Monad m => MonadFresh (StateT (Map String Int) m) where
  uniqueName name = do
    count <- gets . fromMaybe 0 . Map.lookup $ name
    Map.insert name (count + 1)
    return $ name ++ show count
