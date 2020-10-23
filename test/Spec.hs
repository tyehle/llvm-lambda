import Test.Tasty

import ANormSpec
import ExecutionSpec
import LLSpec
import ParseLispSpec

main :: IO ()
main = putStrLn "" >> buildTests >>= defaultMain

buildTests :: IO TestTree
buildTests = do
  executionTests <- findExecutionTests
  return $ testGroup "Tests" [parseLispTests, lowLevelTests, aNormTests, executionTests]
