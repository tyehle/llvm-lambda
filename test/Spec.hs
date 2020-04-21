import Test.Tasty
import Test.Tasty.HUnit

import ANormSpec
import ExecutionSpec
import ParseLispSpec

main :: IO ()
main = putStrLn "" >> buildTests >>= defaultMain

buildTests :: IO TestTree
buildTests = do
  executionTests <- findExecutionTests
  return $ testGroup "Tests" [parseLispTests, aNormTests, executionTests]
