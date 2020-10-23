module LLSpec (lowLevelTests) where

import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit

import LowLevel
import Parsing (parse)

emitLL :: String -> Prog
emitLL input = runConvert (either error id $ parse "test_input" input) Set.empty

checkLowLevel :: String -> Prog -> Assertion
checkLowLevel input expected = emitLL input @?= expected

lowLevelTests :: TestTree
lowLevelTests = testGroup "Low Level Tests"
  [ testCase "letrec" $ checkLowLevel
      "(letrec [x x] x)"
      (Prog
        [ClosureDef "_f0" "_env" ["x"]
          (AppClos (Ref "x") [Ref "x"])]
        (Let "x" (NewClos "_f0" [])
          (AppClos (Ref "x") [Ref "x"])))
  , testCase "lambda" $ checkLowLevel
      "(lambda (x y) y)"
      (Prog
        [ClosureDef "_f0" "_env" ["x", "y"] (Ref "y")]
        (NewClos "_f0" []))
  , testCase "lambda shadowing" $ checkLowLevel
      "(lambda (x) (lambda (x) x))"
      (Prog
        [ ClosureDef "_f0" "_env" ["x"] (NewClos "_f1" [])
        , ClosureDef "_f1" "_env" ["x"] (Ref "x")
        ]
        (NewClos "_f0" []))
  , testCase "lambda nested reference" $ checkLowLevel
      "(lambda (x) (lambda (y) x))"
      (Prog
        [ ClosureDef "_f0" "_env" ["x"] (NewClos "_f1" [Ref "x"])
        , ClosureDef "_f1" "_env" ["y"] (GetEnv "_env" 0)
        ]
        (NewClos "_f0" []))
  , testCase "lambda double nested reference" $ checkLowLevel
      "(lambda (x) (lambda (y) (lambda (z) (+ x y))))"
      (Prog
        [ ClosureDef "_f0" "_env" ["x"] (NewClos "_f1" [Ref "x"])
        , ClosureDef "_f1" "_env" ["y"] (NewClos "_f2" [GetEnv "_env" 0, Ref "y"])
        , ClosureDef "_f2" "_env" ["z"] (Plus (GetEnv "_env" 0) (GetEnv "_env" 1))
        ]
        (NewClos "_f0" []))
  ]