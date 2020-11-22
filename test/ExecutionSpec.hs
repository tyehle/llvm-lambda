module ExecutionSpec (findExecutionTests) where

-- import qualified Data.AttoLisp as L
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory (removeFile)
import System.FilePath (takeBaseName, replaceExtension)
import System.Process (readProcess)
import Test.Tasty
import Test.Tasty.Golden

import qualified ANorm as A
import CodegenMonad (generate)
import Fresh (evalFresh)
import Infer
import qualified LowLevel as LL
import Parsing (parse)


toLLVM :: String -> String -> IO BS.ByteString
toLLVM filename input = evalFresh (A.aNormalizeProg lowLevel >>= generate) Map.empty
  where
    globals = Set.fromList ["printf"]
    ast = either error id $ do
      expr <- parse filename input
      _ <- infer expr
      return expr
    lowLevel = LL.runConvert ast globals


goldenTest :: String -> TestTree
goldenTest tlFile = goldenVsString (takeBaseName tlFile) goldFile $ do
  input <- readFile tlFile
  llvm <- toLLVM tlFile input
  _ <- readProcess "llc-9" ["-O2", "-filetype=asm", "-o", asmFile] $ BSU.toString llvm
  _ <- readProcess "clang" ["-c", "runtime.c"] ""
  _ <- readProcess "clang" [asmFile, "runtime.o", "-o", exeFile] ""
  result <- BLU.fromString <$> readProcess ("./" ++ exeFile) [] ""
  _ <- removeFile asmFile
  _ <- removeFile exeFile
  return result
  where
    goldFile = replaceExtension tlFile ".gold"
    asmFile = replaceExtension tlFile ".s"
    exeFile = replaceExtension tlFile ".out"


findExecutionTests :: IO TestTree
findExecutionTests = do
  tlFiles <- findByExtension [".tl"] "test/golden"
  return $ testGroup "Golden Tests" [goldenTest tlFile | tlFile <- tlFiles]