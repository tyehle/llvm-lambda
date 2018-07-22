module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set

import Expr
import qualified LowLevel as LL
import Codegen

main :: IO ()
-- main = sample >>= BS.putStrLn
main = generate (LL.runConvert closures globals) >>= BS.putStrLn
  where
    globals = Set.fromList ["printf"]
    simple = Plus (Nat 1) (Nat 2)
    ast = Let "x" (Plus (Nat 1) (Nat 2)) $ Minus (Nat 5) (Ref "x")
    -- closure ast
    -- (let (x 319) ((lambda (y) (+ x y)) x))
    closures = Let "x" (Nat 319) $ App (Lambda ["y"] (Plus (Ref "x") (Ref "y"))) [Ref "x"]
