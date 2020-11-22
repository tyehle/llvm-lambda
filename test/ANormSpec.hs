module ANormSpec (aNormTests) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit

import qualified ANorm as A
import Fresh (evalFresh)
import qualified LowLevel as LL
import Parsing (parse)

runNormalization :: LL.Expr -> A.Expr
runNormalization input = evalFresh (A.aNormalizeExpr input) Map.empty

checkNormalization :: LL.Expr -> A.Expr -> Assertion
checkNormalization input expected = runNormalization input @?= expected

emitLL :: String -> LL.Prog
emitLL input = LL.runConvert (either error id $ parse "" input) Set.empty

mainBody :: LL.Prog -> LL.Expr
mainBody (LL.Prog defs body) = body

aNormTests :: TestTree
aNormTests = testGroup "A Normalization Tests"
  [ testCase "plus" $ checkNormalization
      (LL.Plus (LL.Num 1) (LL.Num 2))
      (A.Let "_add_a_0" (A.Num 1)
        (A.Let "_add_b_0" (A.Num 2)
          (A.BinOp A.Add (A.Ref "_add_a_0") (A.Ref "_add_b_0"))))

  , testCase "binding order" $ checkNormalization
      (LL.App "external" [LL.Num 0, LL.Ref "one", LL.Num 2])
      (A.Let "_arg0_0" (A.Num 0)
        (A.Let "_arg2_0" (A.Num 2)
          (A.App "external" [A.Ref "_arg0_0", A.Ref "one", A.Ref "_arg2_0"])))

  , testCase "nested" $ checkNormalization
      (LL.Plus (LL.Plus (LL.Ref "one") (LL.Num 2)) (LL.Plus (LL.Num 3) (LL.Ref "four")))
      (A.Let "_add_a_0" (A.Let "_add_b_0" (A.Num 2)
                          (A.BinOp A.Add (A.Ref "one") (A.Ref "_add_b_0")))
        (A.Let "_add_b_1" (A.Let "_add_a_1" (A.Num 3)
                            (A.BinOp A.Add (A.Ref "_add_a_1") (A.Ref "four")))
          (A.BinOp A.Add (A.Ref "_add_a_0") (A.Ref "_add_b_1"))))

  , testCase "closure" $ checkNormalization
      (LL.AppClos (LL.NewClos "_f0" []) [LL.Num 7])
      (A.Let "_clos_0" (A.NewClos "_f0" []) (A.Let "_arg0_0" (A.Num 7) (A.AppClos (A.Ref "_clos_0") [A.Ref "_arg0_0"])))

  , let input = LL.Prog [LL.ClosureDef "func" "_env" ["n"] (LL.Plus (LL.Ref "n") (LL.Num 5))] (LL.App "func" [LL.Num 1])
        expected = A.Prog [A.ClosureDef "func" "_env" ["n"] (A.Let "_add_b_0" (A.Num 5) (A.BinOp A.Add (A.Ref "n") (A.Ref "_add_b_0")))]
                     (A.Let "_arg0_0" (A.Num 1) (A.App "func" [A.Ref "_arg0_0"]))
    in testCase "prog" $ evalFresh (A.aNormalizeProg input) Map.empty @?= expected
  ]