{-# LANGUAGE OverloadedStrings #-}

module LibCDefs where

import LLVM.AST hiding (functionAttributes)
import LLVM.AST.Global
import LLVM.AST.AddrSpace
import LLVM.AST.FunctionAttribute

{-# ANN module ("HLint: ignore Unnecessary hiding" :: String) #-}

data ExternalDefinition = ExternalDefinition Definition CallableOperand


doubleType :: Type
doubleType = FloatingPointType DoubleFP

intType :: Type
intType = IntegerType 32

longType :: Type
longType = IntegerType 64

charType :: Type
charType = IntegerType 8

boolType :: Type
boolType = IntegerType 1

star :: Type -> Type
star t = PointerType t (AddrSpace 0)


printf :: Global
printf = functionDefaults
  { name = Name "printf"
  , parameters = ([Parameter (star charType) (Name "fmt") []], True)
  , returnType = intType
  }


malloc :: Global
malloc = functionDefaults
  { name = Name "malloc"
  , parameters = ([Parameter longType (Name "size") []], False)
  , returnType = star charType
  }


free :: Global
free = functionDefaults
  { name = Name "free"
  , parameters = ([Parameter (star charType) (Name "ptr") []], False)
  , returnType = VoidType
  }


exit :: Global
exit = functionDefaults
  { name = Name "exit"
  , parameters = ([Parameter intType (Name "exit_code") []], False)
  , returnType = VoidType
  , functionAttributes = [Right NoReturn, Right NoUnwind]
  }
