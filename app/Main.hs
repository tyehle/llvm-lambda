module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set

import Expr
import qualified LowLevel as LL
import Codegen

main :: IO ()
main = generate (LL.runConvert ast globals) >>= BS.putStrLn
  where
    globals = Set.fromList ["printf"]
    simple = Plus (Nat 1) (Plus (Nat 12) (Nat 2))
    ast = Let "x" (Plus (Nat 1) (Nat 2)) $ Minus (Nat 5) (Ref "x")
