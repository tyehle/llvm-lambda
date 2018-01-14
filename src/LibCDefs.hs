{-# LANGUAGE OverloadedStrings #-}

module LibCDefs where

import Data.ByteString.Short (ShortByteString)

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Constant
import LLVM.AST.AddrSpace


data ExternalDefinition = ExternalDefinition Definition CallableOperand


doubleType :: Type
doubleType = FloatingPointType DoubleFP

intType :: Type
intType = IntegerType 32

longType :: Type
longType = IntegerType 64

charType :: Type
charType = IntegerType 8

charStarType :: Type
charStarType = PointerType charType (AddrSpace 0)


printf :: Definition
printf = GlobalDefinition functionDefaults
  { name = Name "printf"
  , parameters = ([Parameter charStarType (Name "fmt") []], True)
  , returnType = intType
  , basicBlocks = []
  }


malloc :: Definition
malloc = GlobalDefinition functionDefaults
  { name = Name "malloc"
  , parameters = ([Parameter longType (Name "size") []], False)
  , returnType = charStarType
  , basicBlocks = []
  }


free :: Definition
free = GlobalDefinition functionDefaults
  { name = Name "free"
  , parameters = ([Parameter charStarType (Name "ptr") []], False)
  , returnType = VoidType
  , basicBlocks = []
  }


externalFunctionOp :: Type -> [Type] -> Bool -> ShortByteString -> CallableOperand
externalFunctionOp retType argTypes varArgs name = Right $ ConstantOperand $ GlobalReference ptrType (Name name)
  where
    ptrType = PointerType (FunctionType retType argTypes varArgs) (AddrSpace 0)
