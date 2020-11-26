{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module RuntimeSpec (makeRuntimeTests) where

import qualified Control.Monad as M (void)
import qualified Data.ByteString.UTF8 as BSU
import System.Directory (createDirectoryIfMissing)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Regex.Posix

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
testArgs = defaultArgs { optimizationFlag = Just "-O3" }


printObj :: (MonadIRBuilder m, MonadModuleBuilder m) => Runtime -> Operand -> m ()
printObj Runtime{header, printObject} obj = do
  ptr <- bitcast obj (ptr header)
  _ <- call printObject [(ptr, [])]
  return ()

printPtr :: (MonadIRBuilder m, MonadModuleBuilder m) => Runtime -> Operand -> m ()
printPtr Runtime{printf} ptr = do
  formatString <- globalStringPtr "%p\n" "ptr_fmt_string"
  call printf [(ConstantOperand formatString, []), (ptr, [])]
  return ()


runtimeTest :: String -> String -> IRBuilderT ModuleBuilder () -> TestTree
runtimeTest name expected prog = testCase name $ do
  createDirectoryIfMissing True dirname
  output <- runModule $ buildModule "testing-module" $ wrapMain prog
  assertBool ("expected: " ++ expected ++ "\n but got: " ++ show output) $ matches output
  where
    dirname = "tmp-test"
    exeFile = dirname ++ "/" ++ name ++ ".out"

    wrapMain :: MonadModuleBuilder m => IRBuilderT m a -> m ()
    wrapMain body = M.void $ function "main" [] i32 $ \[] -> body >> ret (int32 0)

    matches :: String -> Bool
    matches = match $ makeRegexOpts compExtended defaultExecOpt expected

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
    [ let expected = "^obj@.*<\\(nil\\),0000\\|0001\\|0000>\\[0xa431\\]\n$"
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
        runtime@Runtime{printScope} <- defineRuntime
        a <- createInt runtime (int64 0x6dfa)
        pushScope runtime a
        printObj runtime a
        _ <- call printScope []
        return ()

    , let expected = "^\\[(.*)\\]\n\
                     \obj@(.*)<\\(nil\\),.*>\\[0xfa72\\]\n\
                     \obj@\\1<\\2,.*>\\[0x3401\\]\n$"
      in runtimeTest "popScope" expected $ do
        runtime@Runtime{printScope} <- defineRuntime
        a <- createInt runtime (int64 0xfa72)
        b <- createInt runtime (int64 0x3401)
        pushScope runtime b
        pushScope runtime a
        popScope runtime
        call printScope []
        printObj runtime a
        printObj runtime b

    , runtimeTest "setSlot" "obj@.*<.*>\\[0xfe2f\\]\n$" $ do
        runtime@Runtime{header} <- defineRuntime
        a <- createInt runtime (int64 0x6af8)
        val <- inttoptr (int64 0xfe2f) (ptr header)
        setSlot runtime a (int64 0) val
        printObj runtime a

    , runtimeTest "getSlot" "^0x762c\n$" $ do
        runtime <- defineRuntime
        a <- createInt runtime (int64 0x762c)
        val <- getSlot runtime a (int64 0)
        printPtr runtime val

    , let expected = "^obj@.*<\\(nil\\),0000\\|0004\\|0001>\\[0xefe4,0x3cd0,0x8d3d,0x897b\\]\n$"
      in runtimeTest "createClosure" expected $ do
        runtime <- defineRuntime
        f <- inttoptr (int64 0x897b) (ptr i8)
        p <- inttoptr (int64 0xefe4) (ptr i8)
        obj <- createClosure runtime f [p] [int64 0x3cd0, int64 0x8d3d]
        printObj runtime obj

    , let expected = "^obj@.*<.*,0000\\|0003\\|0001>\\[\\0xf330,0x8a2f,0x.{6}]\n$"
      in runtimeTest "callClosure" expected $ do
        runtime <- defineRuntime
        fn <- function "__test_closure_fn" [(ptr (ptr i8), "env")] void $
          \[env] -> printObj runtime env >> retVoid
        p <- inttoptr (int64 0xf330) (ptr i8)
        clos <- createClosure runtime fn [p] [int64 0x8a2f]
        _ <- callClosure runtime clos []
        return ()
    ]