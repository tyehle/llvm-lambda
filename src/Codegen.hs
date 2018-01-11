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

import Expr
import Fresh

generate :: Expr -> IO ByteString
generate expr = withContext $ \ctx -> withModuleFromAST ctx (genModule expr) moduleLLVMAssembly


newtype CodegenT m a =
  CodegenT { codegenState :: StateT CodegenState m a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

data CodegenState = CodegenState
  { instructions :: [Named Instruction]
  , environment :: Map String Operand
  }

type Codegen = CodegenT Identity

execCodegen :: Codegen a -> CodegenState -> CodegenState
execCodegen = execState . codegenState

modifyEnvironment :: MonadState CodegenState m => (Map String Operand -> Map String Operand) -> m ()
modifyEnvironment f = modify $ \s -> s { environment = f . environment $ s }


type FreshCodegen = FreshT Codegen

execFreshCodegen :: FreshCodegen a -> CodegenState
execFreshCodegen = flip execCodegen emptyState . flip evalFreshT Map.empty
  where
    emptyState = CodegenState { instructions=[], environment=Map.empty }


doubleType :: Type
doubleType = FloatingPointType DoubleFP

intType :: Type
intType = IntegerType 32

charType :: Type
charType = IntegerType 8

charStarType :: Type
charStarType = PointerType charType (AddrSpace 0)


genModule :: Expr -> Module
genModule expr = defaultModule { moduleName = "main", moduleDefinitions = [printf, numberFmt, main] }
  where
    printf :: Definition
    printf = GlobalDefinition functionDefaults
      { G.name = Name "printf"
      , G.parameters = ([Parameter charStarType (Name "fmt") []], True)
      , G.returnType = intType
      , G.basicBlocks = []
      }

    printfOp :: CallableOperand
    printfOp = Right $ ConstantOperand $ C.GlobalReference (PointerType (FunctionType intType [charStarType] True) (AddrSpace 0)) (Name "printf")

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

    printResult :: Operand -> FreshCodegen ()
    printResult reg = do
      let fmtString = ConstantOperand $ C.GetElementPtr True (C.GlobalReference (PointerType (ArrayType 4 charType) (AddrSpace 0)) (Name "fmt")) [C.Int 32 0, C.Int 32 0]
      let doPrint = Do $ Call Nothing C [] printfOp [(fmtString, []), (reg, [])] [] []
      modify $ \s -> s { instructions = instructions s ++ [doPrint] }
      return ()


synthExpr :: Expr -> FreshCodegen Operand
synthExpr (Nat n) = return . ConstantOperand . C.Int 32 . fromIntegral $ n
synthExpr (Plus a b) = do
  aName <- synthExpr a
  bName <- synthExpr b
  doInstruction intType $ Add False False aName bName []
synthExpr (Minus a b) = do
  aName <- synthExpr a
  bName <- synthExpr b
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


doInstruction :: Type -> Instruction -> FreshCodegen Operand
doInstruction typ instr = do
  myName <- UnName <$> fresh
  modify $ \s -> s { instructions = instructions s ++ [myName := instr] }
  return $ LocalReference typ myName
