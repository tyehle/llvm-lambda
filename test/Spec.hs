import Test.Tasty
import Test.Tasty.HUnit

import ANormSpec

main :: IO ()
main = putStrLn "" >> defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [aNormTests]
