module Expr where

data Expr = Nat Int
          | Plus Expr Expr
          | Minus Expr Expr
          | Ref String
          | Let String Expr Expr
          deriving (Eq, Show)
