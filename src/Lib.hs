{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Lib where

import qualified Data.ByteString.Char8 as BS

import LLVM.AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.AST.CallingConvention (CallingConvention(C))
import LLVM.AST.Linkage (Linkage(External))
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant as C
import LLVM.Module hiding (Module)

import Expr

example :: Expr
example = Let "x" (Plus (Nat 1) (Nat 2)) $ Minus (Nat 5) (Ref "x")


someIR :: Module
someIR = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = [defPutChar, defAdd, main]
  }
  where
    int :: Type
    int = IntegerType 32
    defAdd :: Definition
    defAdd = GlobalDefinition functionDefaults
      { name = Name "add"
      , parameters = ([Parameter int (Name "a") [], Parameter int (Name "b") []], False)
      , returnType = int
      , basicBlocks =
        [ BasicBlock
            (Name "entry")
            [Name "result" := Add
              False -- no signed wrap
              False -- no unsigned wrap
              (LocalReference int (Name "a"))
              (LocalReference int (Name "b"))
              []]
            (Do $ Ret (Just (LocalReference int (Name "result"))) [])
        ]
      }
    defPutChar :: Definition
    defPutChar = GlobalDefinition functionDefaults
      { name = Name "putchar"
      , linkage = External
      , parameters = ([Parameter int (Name "c") []], False)
      , returnType = int
      , basicBlocks = []
      }
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
      where
        putcharOp = Right $ ConstantOperand $ C.GlobalReference (PointerType (FunctionType int [int] False) (AddrSpace 0)) (Name "putchar")


toLLVM :: Module -> IO BS.ByteString
toLLVM ast = withContext $ \ctx -> withModuleFromAST ctx ast moduleLLVMAssembly
