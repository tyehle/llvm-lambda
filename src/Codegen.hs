{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BS (toShort)
import Data.Map (Map)
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
generate expr = do
  let module_ = testing expr
  -- print . concatMap defBlocks . moduleDefinitions $ module_
  withContext $ \ctx -> withModuleFromAST ctx module_ moduleLLVMAssembly
  where
    defBlocks (GlobalDefinition Function{ G.basicBlocks=blocks }) = blocks
    defBlocks _ = []

newtype CodegenT m a =
  CodegenT { codegenState :: StateT CodegenState m a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

data CodegenState = CodegenState
  { instructions :: [Named Instruction]
  , environment :: Map String Operand
  }

runCodegenT :: CodegenT m a -> CodegenState -> m (a, CodegenState)
runCodegenT = runStateT . codegenState

type Codegen = CodegenT Identity

runCodegen :: Codegen a -> CodegenState -> (a, CodegenState)
runCodegen = runState . codegenState

execCodegen :: Codegen a -> CodegenState -> CodegenState
execCodegen = execState . codegenState

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

emptyModule :: String -> Module
emptyModule label = defaultModule { moduleName = BS.toShort . BS.pack $ label }

testing :: Expr -> Module
testing expr = defaultModule { moduleName = "main", moduleDefinitions = [printf, numberFmt, main] }
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
      -- let fmtString = ConstantOperand $ C.Array charType [C.Int 8 37, C.Int 8 100, C.Int 8 10, C.Int 8 0]
      let fmtString = ConstantOperand $ C.GetElementPtr True (C.GlobalReference (PointerType (ArrayType 4 charType) (AddrSpace 0)) (Name "fmt")) [C.Int 32 0]
      let doPrint = Do $ Call Nothing C [] printfOp [(fmtString, []), (reg, [])] [] []
      modify $ \s -> s { instructions = instructions s ++ [doPrint] }
      return ()

synthExpr :: Expr -> FreshCodegen Operand
synthExpr (Nat n) = return . ConstantOperand . C.Int 32 . fromIntegral $ n
synthExpr (Plus a b) = do
  aName <- synthExpr a
  bName <- synthExpr b
  -- myName <- UnName <$> fresh
  myName <- Name . BS.toShort . BS.pack <$> uniqueName "plusResult"
  let instruction = myName := Add False False aName bName []
  modify $ \s -> s { instructions = instructions s ++ [instruction] }
  return $ LocalReference intType myName


{-
main :: Definition
main = GlobalDefinition functionDefaults
  { name = Name "main"
  , parameters = ([], False)
  , returnType = VoidType
  , basicBlocks =
    [ BasicBlock
        (Name "entry") -- block name
        [ Do $ Call Nothing C [] putcharOp [(ConstantOperand $ C.Int 32 33, [])] [] [] -- instructions
        , Do $ Call Nothing C [] putcharOp [(ConstantOperand $ C.Int 32 10, [])] [] []
        ]
        (Do $ Ret Nothing []) -- return value
    ]
  }
-}
