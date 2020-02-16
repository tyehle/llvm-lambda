module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set

import Expr
import qualified LowLevel as LL
import Codegen

main :: IO ()
-- main = sample >>= BS.putStrLn
main = generate (LL.runConvert ifs globals) >>= BS.putStrLn
  where
    globals = Set.fromList ["printf"]
    -- simple = Plus (Nat 1) (Nat 2)
    -- ast = Let "x" (Plus (Nat 1) (Nat 2)) $ Minus (Nat 5) (Ref "x")
    -- closure ast
    -- ((let (x 319) ((lambda (y) (+ x y)) x)) (let (x 319) ((lambda (y) (+ x y)) x)))
    closures = Let "x" (Nat 319) $ App (Lambda ["y"] (Plus (Ref "x") (Ref "y"))) [Ref "x"]
    -- (let (mk-add (lambda (n) (lambda (x) (+ x n))))
    --   (let (a (mk-add 1))
    --     (let (b (mk-add 2))
    --       (+ (a 3) (b 5)))))
    complex = Let "mk-add" (Lambda ["n"] (Lambda ["x"] (Plus (Ref "x") (Ref "n")))) $
                Let "a" (App (Ref "mk-add") [Nat 1]) $
                  Let "b" (App (Ref "mk-add") [Nat 2]) $
                    Plus (App (Ref "a") [Nat 3]) (App (Ref "b") [Nat 5])
    -- (if0 0 1 2)
    ifs = If0 (Nat 0) (Nat 1) (Nat 2)
