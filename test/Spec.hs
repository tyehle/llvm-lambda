import Test.Tasty

import ANormSpec
import ExecutionSpec
import LLSpec
import ParseLispSpec
import RuntimeSpec

main :: IO ()
main = putStrLn "" >> buildTests >>= defaultMain

buildTests :: IO TestTree
buildTests = do
  executionTests <- findExecutionTests
  runtimeTests <- makeRuntimeTests
  return $ testGroup "Tests"
    [ parseLispTests
    , lowLevelTests
    , aNormTests
    , runtimeTests
    , executionTests
    ]
