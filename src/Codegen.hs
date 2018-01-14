{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Codegen where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BS (toShort)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity

import LLVM.AST
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import LLVM.AST.AddrSpace (AddrSpace(AddrSpace))
import LLVM.AST.CallingConvention (CallingConvention(C))
import LLVM.AST.Linkage (Linkage(Private))
import LLVM.Context
import LLVM.Module hiding (Module)

import LowLevel
import Fresh
import LibCDefs
import Scope

generate :: Prog -> IO ByteString
generate expr = withContext $ \ctx -> withModuleFromAST ctx (genModule expr) moduleLLVMAssembly


newtype CodegenT m a =
  CodegenT { codegenState :: StateT CodegenState m a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

type Env = Map String (Operand, Type)

data CodegenState = CodegenState
  { defs :: [Definition]
  , instructions :: [Named Instruction]
  , environment :: Env
  }

type Codegen = CodegenT Identity

execCodegen :: Codegen a -> CodegenState -> CodegenState
execCodegen = execState . codegenState

modifyEnvironment :: MonadState CodegenState m => (Env -> Env) -> m ()
modifyEnvironment f = modify $ \s -> s { environment = f . environment $ s }

appendInstruction :: MonadState CodegenState m => Named Instruction -> m ()
appendInstruction instr = modify $ \s -> s { instructions = instructions s ++ [instr] }


type FreshCodegen = FreshT Codegen

execFreshCodegen :: FreshCodegen a -> CodegenState
execFreshCodegen = flip execCodegen emptyState . flip evalFreshT Map.empty
  where
    emptyState = CodegenState { defs=[], instructions=[], environment=Map.empty }


genModule :: Prog -> Module
genModule (Prog defs expr) = defaultModule { moduleName = "main", moduleDefinitions = [printf, numberFmt, main] }
  where
    numberFmt :: Definition
    numberFmt = GlobalDefinition globalVariableDefaults
      { G.name = Name "fmt"
      , G.linkage = Private
      , G.isConstant = True
      , G.type' = ArrayType 4 charType
      , G.initializer = Just $ C.Array charType [C.Int 8 37, C.Int 8 100, C.Int 8 10, C.Int 8 0]
      }

    main :: Definition
    main = GlobalDefinition functionDefaults
      { G.name = Name "main"
      , G.parameters = ([], False)
      , G.returnType = VoidType
      , G.basicBlocks = [BasicBlock (Name "entry") body (Do $ Ret Nothing [])]
      }

    body :: [Named Instruction]
    body = instructions . execFreshCodegen $ synthExpr expr >>= printResult

    printResult :: (Operand, Type) -> FreshCodegen ()
    printResult (reg, IntegerType 32) = do
      let refType = PointerType (ArrayType 4 charType) (AddrSpace 0)
      let fmtArg = ConstantOperand $ C.GetElementPtr True (C.GlobalReference refType (Name "fmt")) [C.Int 32 0, C.Int 32 0]
      appendInstruction $ Do $ callExternal (externalFunctionOp intType [charStarType] True "printf") [fmtArg, reg]
    printResult (_, badType) = error $ "Can't print a result with type " ++ show badType


synthExpr :: Expr -> FreshCodegen (Operand, Type)
synthExpr (Num n) = return . flip (,) intType . ConstantOperand . C.Int 32 . fromIntegral $ n
synthExpr (Plus a b) = do
  (aName, _) <- synthExpr a
  (bName, _) <- synthExpr b
  doInstruction intType $ Add False False aName bName []
synthExpr (Minus a b) = do
  (aName, _) <- synthExpr a
  (bName, _) <- synthExpr b
  doInstruction intType $ Sub False False aName bName []
synthExpr (Let name binding body) = do
  oldEnv <- gets environment
  bindingName <- synthExpr binding
  modifyEnvironment $ Map.insert name bindingName
  resultName <- synthExpr body
  modifyEnvironment $ const oldEnv
  return resultName
synthExpr (Ref name) = do
  env <- gets environment
  return $ env ! name
synthExpr (App func args) = undefined


doInstruction :: Type -> Instruction -> FreshCodegen (Operand, Type)
doInstruction typ instr = do
  myName <- fresh
  modify $ \s -> s { instructions = instructions s ++ [myName := instr] }
  return . flip (,) typ $ LocalReference typ myName


callExternal :: CallableOperand -> [Operand] -> Instruction
callExternal func argOps = Call Nothing C [] func args [] []
  where
    args = map (flip (,) []) argOps
