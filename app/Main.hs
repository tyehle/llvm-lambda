module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import System.Environment (getArgs)

import Codegen
import Expr
import qualified LowLevel as LL
import Parsing


getInput :: IO String
getInput = do
  args <- getArgs
  case args of
    (arg:_) -> pure arg
    [] -> getContents


runPipeline :: String -> IO BS.ByteString
runPipeline input = generate (LL.runConvert (either error id $ parse input) globals)
  where
    globals = Set.fromList ["printf"]


main :: IO ()
main = do
  input <- getInput
  runPipeline input >>= BS.putStrLn


simple, ast, closures, ifs :: String
simple = "(+ 1 2)"
ast = "(let (x (+ 1 2)) (- 5 x))"
closures = "(let (x 319) ((lambda (y) (+ x y)) x))"
complex = "(let (mk-add (lambda (n) (lambda (x) (+ x n)))) \
          \  (let (a (mk-add 1)) \
          \    (let (b (mk-add 2)) \
          \      (+ (a 3) (b 5)))))"
ifs = "(if0 0 1 2)"

