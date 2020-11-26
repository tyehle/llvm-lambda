module ExecutionSpec (findExecutionTests) where

import qualified Data.ByteString.Lazy as BL
import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty
import Test.Tasty.Golden

import CmdLine
import CmdLineArgs


testArgs :: Args
testArgs = defaultArgs { optimizationFlag = Just "-O3" }


goldenTest :: String -> TestTree
goldenTest tlFile = goldenVsString (takeBaseName tlFile) goldFile $ do
  either error BL.fromStrict <$> produceOutput args
  where
    goldFile = replaceExtension tlFile ".gold"
    args = testArgs { inputFile = Just tlFile }


findExecutionTests :: IO TestTree
findExecutionTests = do
  tlFiles <- findByExtension [".tl"] "test/golden"
  return $ testGroup "Golden Tests" [goldenTest tlFile | tlFile <- tlFiles]