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
emitLL input = LL.runConvert (either error id $ parse input) Set.empty

mainBody :: LL.Prog -> LL.Expr
mainBody (LL.Prog defs body) = body

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

  , let input = LL.Prog [LL.ClosureDef "func" "_env" ["n"] (LL.Plus (LL.Ref "n") (LL.Num 5))] (LL.App "func" [LL.Num 1])
        expected = A.Prog [A.ClosureDef "func" "_env" ["n"] (A.Let "_add_b_0" (A.Num 5) (A.Plus (A.Ref "n") (A.Ref "_add_b_0")))]
                     (A.Let "_arg0_0" (A.Num 1) (A.App "func" [(A.Ref "_arg0_0")]))
    in testCase "prog" $ (evalFresh (A.aNormalizeProg input) Map.empty) @?= expected

  , let input = emitLL "(let (f (lambda (n rec) (if0 n n (rec (- n 1))))) \
                       \  (f 10 f))"
        expected = A.Prog [A.ClosureDef "_f0" "_env" ["n", "rec"]
                            (A.If0 (A.Ref "n")
                              (A.Atomic (A.Ref "n"))
                              (A.Let "_arg0_0" (A.Let "_sub_b_0" (A.Num 1) (A.Minus (A.Ref "n") (A.Ref "_sub_b_0")))
                                (A.AppClos True (A.Ref "rec") [A.Ref "_arg0_0"])))]
                     (A.Let "f" (A.NewClos "_f0" [])
                       (A.Let "_arg0_1" (A.Num 10)
                         (A.AppClos False (A.Ref "f") [A.Ref "_arg0_1", A.Ref "f"])))
    in testCase "tail calls" $ (evalFresh (A.aNormalizeProg input) Map.empty) @?= expected
  ]