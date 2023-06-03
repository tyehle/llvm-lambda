{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module CodegenMonad where

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import Control.Monad.State.Strict hiding (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as CBS (pack, unpack)
import qualified Data.ByteString.Short as SBS (toShort)
import Data.Map (Map)
import qualified Data.Map as Map

import LLVM.AST ( Module, Operand(ConstantOperand), mkName )
import qualified LLVM.AST.IntegerPredicate as Pred ( IntegerPredicate(EQ) )
import LLVM.AST.Type
import qualified LLVM.AST.Constant as C
import LLVM.Context ( withContext )
import LLVM.Module ( moduleLLVMAssembly, withModuleFromAST )

import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import ANorm
import Fresh
import Pretty
import RuntimeDefs


generate :: Prog -> Fresh (IO ByteString)
generate prog = do
  llvmModule <- either error id <$> runExceptT (genModule prog)
  return $ withContext $ \ctx -> withModuleFromAST ctx llvmModule moduleLLVMAssembly


serialize :: Module -> IO String
serialize mod = withContext $ \ctx -> withModuleFromAST ctx mod (fmap CBS.unpack . moduleLLVMAssembly)


genModule :: Prog -> ExceptT String Fresh Module
genModule prog = buildModuleT "tl-module" $ do
  runtime <- defineRuntime
  flip runReaderT runtime $ flip evalStateT Map.empty $ genProgram prog


type Env = Map String Operand

type ModuleState = StateT Env (ReaderT Runtime (ModuleBuilderT (ExceptT String Fresh)))

type Codegen = IRBuilderT ModuleState


genProgram :: Prog -> ModuleState ()
genProgram (Prog progDefs expr) = do
  -- add all defs to the env
  mapM_ addDefToEnv progDefs
  -- define all functions
  mapM_ genDef progDefs
  -- define main
  function "main" [] i32 mainBody
  return ()
  where
    mainBody :: [Operand] -> Codegen ()
    mainBody [] = do
      runtime@Runtime{printf, printObject, printScope} <- ask
      resultObj <- genExpr expr
      result <- getInt runtime resultObj
      formatString <- globalStringPtr "%d\n" "main_fmt_string"
      call (FunctionType i32 [ptr] True) printf [(ConstantOperand formatString, []), (result, [])]
      -- force the compiler to keep debugging functions even when --debug-runtime is off
      -- uncomment for debugging binaries
      -- _ <- call (FunctionType void [ptr] False) printObject [(resultObj, [])]
      -- _ <- call (FunctionType void [] False) printScope []
      ret (int32 0)

    addDefToEnv :: Def -> ModuleState ()
    addDefToEnv (ClosureDef name _ argNames _) = do
      Runtime{header} <- ask
      let operand = ConstantOperand $ C.GlobalReference (mkName name)
          numArgs = 1 + length argNames
          -- ty = ptr $ FunctionType (ptr header) (replicate numArgs $ ptr header) False
      modify $ Map.insert name operand


genDef :: Def -> ModuleState ()
genDef (ClosureDef name envName argNames body) = do
  Runtime{header} <- ask
  let params = [(ptr, paramName name) | name <- envName : argNames]
  function (mkName name) params ptr defineBody
  return ()
  where
    paramName :: String -> ParameterName
    paramName = ParameterName . SBS.toShort . CBS.pack

    defineBody :: [Operand] -> Codegen ()
    defineBody args = do
      runtime <- ask
      oldEnv <- get
      mapM_ modify $ zipWith Map.insert (envName:argNames) args
      mapM_ (pushScope runtime) args
      let cleanup = mapM_ (const $ popScope runtime) args
      genExprTailPosition body cleanup <* put oldEnv


genExprTailPosition :: Expr -> Codegen () -> Codegen ()
genExprTailPosition expr cleanup = contextualize expr $ case expr of
  If0 cond tBranch fBranch -> do
    runtime <- ask
    condValue <- genAExpr cond >>= getInt runtime
    comp <- icmp Pred.EQ condValue (int64 0)
    trueLabel <- uniqueName "trueBlock"
    falseLabel <- uniqueName "falseBlock"
    condBr comp trueLabel falseLabel
    -- true block
    emitBlockStart trueLabel
    genExprTailPosition tBranch cleanup
    -- false block
    emitBlockStart falseLabel
    genExprTailPosition fBranch cleanup

  Let name binding body -> do
    runtime <- ask
    oldEnv <- get
    bindingValue <- genExpr binding
    modify $ Map.insert name bindingValue
    pushScope runtime bindingValue
    genExprTailPosition body (popScope runtime >> cleanup) <* put oldEnv

  App name args -> undefined

  AppClos fn args -> do
    runtime <- ask
    fnValue <- genAExpr fn
    argValues <- mapM genAExpr args
    cleanup
    result <- callClosure runtime fnValue argValues
    ret result

  other -> do
    result <- genExpr other
    cleanup
    ret result


genExpr :: Expr -> Codegen Operand
genExpr expr = contextualize expr $ case expr of
  Num n -> do
    runtime <- ask
    createInt runtime (int64 (fromIntegral n))

  BinOp op a b -> do
    runtime <- ask
    aVal <- genAExpr a >>= getInt runtime
    bVal <- genAExpr b >>= getInt runtime
    resultValue <- case op of
      Add -> add aVal bVal
      Sub -> sub aVal bVal
      Mul -> mul aVal bVal
      Div -> sdiv aVal bVal
    createInt runtime resultValue

  If0 cond tBranch fBranch -> do
    runtime <- ask
    condValue <- genAExpr cond >>= getInt runtime
    comp <- icmp Pred.EQ condValue (int64 0)
    trueLabel <- uniqueName "trueBlock"
    falseLabel <- uniqueName "falseBlock"
    doneLabel <- uniqueName "doneBlock"
    condBr comp trueLabel falseLabel
    -- true block
    emitBlockStart trueLabel
    tRes <- genExpr tBranch
    br doneLabel
    -- false block
    emitBlockStart falseLabel
    fRes <- genExpr fBranch
    br doneLabel
    -- done block
    emitBlockStart doneLabel
    phi [(tRes, trueLabel), (fRes, falseLabel)]

  Let name binding body -> do
    runtime <- ask
    oldEnv <- get
    bindingValue <- genExpr binding
    modify $ Map.insert name bindingValue
    pushScope runtime bindingValue
    result <- genExpr body
    popScope runtime
    put oldEnv
    return result

  App name args -> undefined

  AppClos fn args -> do
    runtime <- ask
    fnValue <- genAExpr fn
    argValues <- mapM genAExpr args
    callClosure runtime fnValue argValues

  NewClos fnName bindings -> do
    runtime <- ask
    maybeFn <- gets $ Map.lookup fnName
    fn <- maybe (throwError $ "Undefined function: " ++ fnName) return maybeFn
    bindingValues <- mapM genAExpr bindings
    createClosure runtime fn bindingValues []

  Atomic a -> genAExpr a


genAExpr :: AExpr -> Codegen Operand
genAExpr expr = contextualize expr $ case expr of
  Ref name -> do
    value <- gets $ Map.lookup name
    maybe (throwError $ "Undefined variable: " ++ name) return value

  GetEnv envName index -> do
    env <- genAExpr (Ref envName)
    runtime <- ask
    getSlot runtime env (int64 index)
