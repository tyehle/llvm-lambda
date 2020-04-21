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
import Codegen (generate)
import Expr
import Fresh (evalFresh)
import qualified LowLevel as LL
import Parsing (parse)


toLLVM :: Expr -> IO BS.ByteString
toLLVM input = evalFresh (A.aNormalizeProg lowLevel >>= generate) Map.empty
  where
    globals = Set.fromList ["printf"]
    lowLevel = LL.runConvert input globals


goldenTest :: String -> TestTree
goldenTest tlFile = goldenVsString (takeBaseName tlFile) goldFile $ do
  input <- readFile tlFile
  let ast = either error id $ parse tlFile input
  llvm <- toLLVM ast
  _ <- readProcess "llc-5.0" ["-O2", "-filetype=asm", "-o", asmFile] $ BSU.toString llvm
  _ <- readProcess "clang" ["-c", "gc.c"] ""
  _ <- readProcess "clang" [asmFile, "gc.o", "-o", exeFile] ""
  _ <- removeFile asmFile
  result <- BLU.fromString <$> readProcess ("./" ++ exeFile) [] ""
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