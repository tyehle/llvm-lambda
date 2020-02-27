{-# LANGUAGE OverloadedStrings #-}

module LibCDefs where

import LLVM.AST hiding (functionAttributes)
import LLVM.AST.Global
import LLVM.AST.AddrSpace
import LLVM.AST.FunctionAttribute

{-# ANN module ("HLint: ignore Unnecessary hiding" :: String) #-}

data ExternalDefinition = ExternalDefinition Definition CallableOperand


star :: Type -> Type
star t = PointerType t (AddrSpace 0)


printf :: Global
printf = functionDefaults
  { name = Name "printf"
  , parameters = ([Parameter (star $ IntegerType 8) (Name "fmt") []], True)
  , returnType = IntegerType 32
  }


allocate :: Global
allocate = functionDefaults
  { name = Name "__tl_allocate"
  , parameters = ([Parameter (IntegerType 64) (Name "bytes") []], False)
  , returnType = star $ IntegerType 8
  }


pushScope :: Global
pushScope = functionDefaults
  { name = Name "__push_scope"
  , parameters = ([Parameter (star $ IntegerType 32) (Name "loc") []], False)
  , returnType = VoidType
  }


popScope :: Global
popScope = functionDefaults
  { name = Name "__pop_scope"
  , parameters = ([], False)
  , returnType = VoidType
  }


exit :: Global
exit = functionDefaults
  { name = Name "exit"
  , parameters = ([Parameter (IntegerType 32) (Name "exit_code") []], False)
  , returnType = VoidType
  , functionAttributes = [Right NoReturn, Right NoUnwind]
  }
