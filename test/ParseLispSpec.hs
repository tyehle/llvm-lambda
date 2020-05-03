module ParseLispSpec (parseLispTests) where

import Data.Either
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

import ParseLisp

checkParser :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
checkParser parser input expected = result @?= Right expected
  where
    result = parse parser "testInput" input

parseLispTests :: TestTree
parseLispTests = testGroup "Lisp Parser Tests"
  [ testCase "parse num" $ checkParser expression "12" (Number 12)
  , testCase "parse num" $ checkParser integerLit "-4" (-4)
  , testCase "parse float" $ checkParser expression "123.3e-12" (Float 123.3e-12)
  , testCase "parse symbol" $ checkParser expression "-" (Symbol "-")
  , testCase "parse symbol" $ checkParser expression "\"asdf\"" (String "asdf")
  , testCase "parse symbol" $ checkParser expression "( + 3 -4 )" (List [Symbol "+", Number 3, Number (-4)])
  , testCase "comment" $ checkParser wholeFile ";a\n12\n;b\n" (Number 12)
  , testCase "block comment" $ checkParser wholeFile "#;(asd {} asd)12#;()" (Number 12)
  ]