{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module RuntimeSpec (makeRuntimeTests) where

import qualified Control.Monad as M (void)
import qualified Data.ByteString.UTF8 as BSU
import System.Directory (createDirectoryIfMissing)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Regex.PCRE

import LLVM.AST (Module)
import LLVM.AST.Type
import LLVM.AST.Operand
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant

import CmdLine
import CmdLineArgs
import CodegenMonad
import RuntimeDefs


testArgs :: Args
testArgs = defaultArgs { optimizationFlag = Just "-O3", debugRuntime = False }


printObj :: (MonadIRBuilder m, MonadModuleBuilder m) => Runtime -> Operand -> m ()
printObj Runtime{header, printObject} obj = do
  objPtr <- bitcast obj ptr
  _ <- call (FunctionType void [ptr] False) printObject [(objPtr, [])]
  return ()

printPtr :: (MonadIRBuilder m, MonadModuleBuilder m) => Runtime -> Operand -> m ()
printPtr Runtime{printf} value = do
  formatString <- globalStringPtr "%p\n" "ptr_fmt_string"
  call (FunctionType i32 [ptr] True) printf [(ConstantOperand formatString, []), (value, [])]
  return ()

callPrintScope :: (MonadIRBuilder m, MonadModuleBuilder m) => Runtime -> m ()
callPrintScope Runtime{printScope} = do
  _ <- call (FunctionType void [] False) printScope []
  return ()


runtimeTest :: String -> String -> IRBuilderT ModuleBuilder () -> TestTree
runtimeTest name expected prog = testCase name $ do
  createDirectoryIfMissing True dirname
  output <- runModule $ buildModule "testing-module" $ wrapMain prog
  assertBool ("expected: " ++ show expected ++ "\n but got: " ++ show output) $ matches output
  where
    dirname = "tmp-test"
    exeFile = dirname ++ "/" ++ name ++ ".out"

    wrapMain :: MonadModuleBuilder m => IRBuilderT m a -> m ()
    wrapMain body = M.void $ function "main" [] i32 $ \[] -> body >> ret (int32 0)

    matches :: String -> Bool
    matches actual = actual =~ expected
    -- matches = match $ makeRegexOpts compExtended defaultExecOpt expected

    runModule :: Module -> IO String
    runModule llvmModule = do
      let args = testArgs { inputFile = Just name }
      llvm <- BSU.fromString <$> serialize llvmModule
      result <- optimize args llvm >>= assemble "obj" args >>= link args exeFile >>= execute
      return $ BSU.toString result


makeRuntimeTests :: IO TestTree
makeRuntimeTests = do
  compileRuntimeC testArgs
  return $ testGroup "Runtime Tests"
    [ let expected = "^obj@.*<0x0,0000\\|0001\\|0000>\\[0xa431\\]\n$"
      in runtimeTest "createInt" expected $ do
        runtime <- defineRuntime
        n <- createInt runtime (int64 0xa431)
        printObj runtime n

    , runtimeTest "getInt" "^0xb241\n$" $ do
        runtime <- defineRuntime
        a <- createInt runtime (int64 0xb241)
        n <- getInt runtime a
        printPtr runtime n

    , let expected = "^obj@(.*)<.*>\\[0x6dfa\\]\n\
                     \\\[\\1\\]\n$"
      in runtimeTest "pushScope" expected $ do
        runtime <- defineRuntime
        a <- createInt runtime (int64 0x6dfa)
        pushScope runtime a
        printObj runtime a
        callPrintScope runtime

    , let expected = "^obj@(.*)<0x0,.*>\\[0xfa72\\]\n\
                     \obj@(.*)<\\1,.*>\\[0x3401\\]\n\
                     \\\[\\2\\]$"
      in runtimeTest "popScope" expected $ do
        runtime <- defineRuntime
        a <- createInt runtime (int64 0xfa72)
        b <- createInt runtime (int64 0x3401)
        pushScope runtime b
        pushScope runtime a
        popScope runtime
        printObj runtime a
        printObj runtime b
        callPrintScope runtime

    , runtimeTest "setSlot" "obj@.*<.*>\\[0xfe2f\\]\n$" $ do
        runtime <- defineRuntime
        a <- createInt runtime (int64 0x6af8)
        val <- inttoptr (int64 0xfe2f) ptr
        setSlot runtime a (int64 0) val
        printObj runtime a

    , runtimeTest "getSlot" "^0x762c\n$" $ do
        runtime <- defineRuntime
        a <- createInt runtime (int64 0x762c)
        val <- getSlot runtime a (int64 0)
        printPtr runtime val

    , let expected = "^obj@.*<0x0,0000\\|0004\\|0001>\\[0xefe4,0x3cd0,0x8d3d,0x897b\\]\n$"
      in runtimeTest "createClosure" expected $ do
        runtime <- defineRuntime
        f <- inttoptr (int64 0x897b) ptr
        p <- inttoptr (int64 0xefe4) ptr
        obj <- createClosure runtime f [p] [int64 0x3cd0, int64 0x8d3d]
        printObj runtime obj

    , let expected = "^obj@.*<.*,0000\\|0003\\|0001>\\[0xf330,0x8a2f,0x.{9}\\]\n$"
      in runtimeTest "callClosure" expected $ do
        runtime <- defineRuntime
        fn <- function "__test_closure_fn" [(ptr, "env")] void $
          \[env] -> printObj runtime env >> retVoid
        p <- inttoptr (int64 0xf330) ptr
        clos <- createClosure runtime fn [p] [int64 0x8a2f]
        _ <- callClosure runtime clos []
        return ()

      , let expected = "^obj@(.*)<0x0,0000\\|0001\\|0000>\\[0xe0f5\\]\n\
                       \obj@(.*)<\\1,0000\\|0002\\|0001>\\[\\1,0x535c\\]\n\
                       \obj@(.*)<\\2,0001\\|0001\\|0000>\\[0x3c7b\\]\n\
                       \obj@(.*)<\\3,0001\\|0002\\|0001>\\[\\3,0x5ec7\\]\n\
                       \obj@(.*)<\\4,0001\\|0002\\|0001>\\[\\4,0xf40a\\]\n$"
        in runtimeTest "markObjects" expected $ do
          runtime@Runtime{markObjects} <- defineRuntime
          ep <- inttoptr (int64 0xe0f5) ptr
          dp <- inttoptr (int64 0x535c) ptr
          cp <- inttoptr (int64 0x3c7b) ptr
          bp <- inttoptr (int64 0x5ec7) ptr
          ap <- inttoptr (int64 0xf40a) ptr
          e <- createClosure runtime ep [] []
          d <- createClosure runtime dp [e] []
          c <- createClosure runtime cp [] []
          b <- createClosure runtime bp [c] []
          a <- createClosure runtime ap [b] []
          call (FunctionType void [ptr] False) markObjects [(a, [])]
          printObj runtime e
          printObj runtime d
          printObj runtime c
          printObj runtime b
          printObj runtime a

      , let expected = "^obj@(.*)<0x0,0000\\|0001\\|0000>\\[0xe0f5\\]\n\
                       \obj@(.*)<\\1,0000\\|0001\\|0000>\\[0x3c7b\\]\n\
                       \obj@(.*)<\\2,0000\\|0002\\|0001>\\[\\2,0x5ec7\\]\n\
                       \obj@(.*)<\\3,0000\\|0002\\|0001>\\[\\3,0xf40a\\]\n\
                       \\\[\\1,\\4\\]\n$"
        in runtimeTest "runGC" expected $ do
          runtime@Runtime{runGC, printScope} <- defineRuntime
          ep <- inttoptr (int64 0xe0f5) ptr
          dp <- inttoptr (int64 0x535c) ptr
          cp <- inttoptr (int64 0x3c7b) ptr
          bp <- inttoptr (int64 0x5ec7) ptr
          ap <- inttoptr (int64 0xf40a) ptr
          e <- createClosure runtime ep [] []
          d <- createClosure runtime dp [e] []
          c <- createClosure runtime cp [] []
          b <- createClosure runtime bp [c] []
          a <- createClosure runtime ap [b] []
          pushScope runtime a
          pushScope runtime e
          call (FunctionType void [] False) runGC []
          printObj runtime e
          printObj runtime c
          printObj runtime b
          printObj runtime a
          call (FunctionType void [] False) printScope []
          return ()
    ]