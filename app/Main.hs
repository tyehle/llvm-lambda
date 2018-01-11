module Main where

import qualified Data.ByteString.Char8 as BS

import Codegen
import Expr

main :: IO ()
main = generate simple >>= BS.putStrLn
  where
    simple = Plus (Nat 1) (Plus (Nat 12) (Nat 2))
    ast = Let "x" (Plus (Nat 1) (Nat 2)) $ Minus (Nat 5) (Ref "x")
