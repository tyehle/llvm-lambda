module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment (getArgs)

import qualified ANorm as A
import CodegenMonad
import Expr
import Fresh (evalFresh)
import Infer
import qualified LowLevel as LL
import Parsing
import Pretty


getInput :: IO String
getInput = do
  args <- getArgs
  case args of
    (arg:_) -> pure arg
    [] -> getContents


runPipeline :: String -> IO BS.ByteString
runPipeline input = evalFresh (A.aNormalizeProg lowLevel >>= generate) Map.empty
  where
    globals = Set.fromList ["printf"]
    ast = either error id $ do
      expr <- parse "input" input
      _ <- infer expr
      return expr
    lowLevel = LL.runConvert ast globals


showLL :: String -> String
showLL input = pretty lowLevel
  where
    globals = Set.fromList ["printf"]
    ast = either error id $ do
      expr <- parse "input" input
      _ <- infer expr
      return expr
    lowLevel = LL.runConvert ast globals



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

