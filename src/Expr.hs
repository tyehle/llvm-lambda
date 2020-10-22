module Expr where

import qualified Data.Set as Set

import Scope


data Expr = Nat Int
          | BinOp BinOp Expr Expr
          | If0 Expr Expr Expr
          | Ref String
          | Let String Expr Expr
          | Lambda [String] Expr
          | App Expr [Expr]
          deriving (Eq, Ord, Show)


data BinOp
  = Add
  | Sub
  | Mul
  | Div
  --  | Mod
  deriving (Eq, Ord, Show)


instance Scope Expr where
  freeVars (Nat _) = Set.empty
  freeVars (BinOp _ a b) = Set.union (freeVars a) (freeVars b)
  freeVars (If0 c t f) = Set.union (freeVars c) $ Set.union (freeVars t) (freeVars f)
  freeVars (Ref name) = Set.singleton name
  freeVars (Let name binding body) = Set.union (freeVars binding) $ Set.delete name (freeVars body)
  freeVars (Lambda args body) = freeVars body `Set.difference` Set.fromList args
  freeVars (App fn args) = freeVars fn `Set.union` Set.unions (map freeVars args)
