module ANormSpec (aNormTests) where

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

import Fresh (evalFresh)
import qualified LowLevel as LL
import qualified ANorm as A

runNormalization :: LL.Expr -> A.Expr
runNormalization input = evalFresh (A.aNormalizeExpr input) Map.empty

checkNormalization :: LL.Expr -> A.Expr -> Assertion
checkNormalization input expected = runNormalization input @?= expected

aNormTests :: TestTree
aNormTests = testGroup "A Normalization Tests"
  [ testCase "plus" $ checkNormalization
      (LL.Plus (LL.Num 1) (LL.Num 2))
      (A.Let "_add_a_0" (A.Num 1)
        (A.Let "_add_b_0" (A.Num 2)
          (A.Plus (A.Ref "_add_a_0") (A.Ref "_add_b_0"))))

  , testCase "binding order" $ checkNormalization
      (LL.App "external" [LL.Num 0, LL.Ref "one", LL.Num 2])
      (A.Let "_arg0_0" (A.Num 0)
        (A.Let "_arg2_0" (A.Num 2)
          (A.App "external" [A.Ref "_arg0_0", A.Ref "one", A.Ref "_arg2_0"])))

  , testCase "nested" $ checkNormalization
      (LL.Plus (LL.Plus (LL.Ref "one") (LL.Num 2)) (LL.Plus (LL.Num 3) (LL.Ref "four")))
      (A.Let "_add_a_0" (A.Let "_add_b_0" (A.Num 2)
                          (A.Plus (A.Ref "one") (A.Ref "_add_b_0")))
        (A.Let "_add_b_1" (A.Let "_add_a_1" (A.Num 3)
                            (A.Plus (A.Ref "_add_a_1") (A.Ref "four")))
          (A.Plus (A.Ref "_add_a_0") (A.Ref "_add_b_1"))))
  ]