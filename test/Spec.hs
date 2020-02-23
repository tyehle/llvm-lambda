import Test.Tasty
import Test.Tasty.HUnit

import ANormSpec
import ExecutionSpec

main :: IO ()
main = putStrLn "" >> buildTests >>= defaultMain

buildTests :: IO TestTree
buildTests = do
  executionTests <- findExecutionTests
  return $ testGroup "Tests" [aNormTests, executionTests]
