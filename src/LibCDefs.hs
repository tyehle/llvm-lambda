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


printf :: Global
printf = functionDefaults
  { name = Name "printf"
  , parameters = ([Parameter charStarType (Name "fmt") []], True)
  , returnType = intType
  , basicBlocks = []
  }


malloc :: Global
malloc = functionDefaults
  { name = Name "malloc"
  , parameters = ([Parameter longType (Name "size") []], False)
  , returnType = charStarType
  , basicBlocks = []
  }


free :: Global
free = functionDefaults
  { name = Name "free"
  , parameters = ([Parameter charStarType (Name "ptr") []], False)
  , returnType = VoidType
  , basicBlocks = []
  }


globalFOp :: Global -> CallableOperand
globalFOp def = Right $ ConstantOperand $ GlobalReference ptrType (name def)
  where
    ptrType = PointerType (FunctionType retType argTypes varArgs) (AddrSpace 0)
    retType = returnType def
    argTypes = map typeOf . fst . parameters $ def
    typeOf (Parameter typ _ _) = typ
    varArgs = snd . parameters $ def
