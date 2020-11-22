{-# LANGUAGE OverloadedStrings #-}

module LibCDefs where

import LLVM.AST hiding (functionAttributes)
import LLVM.AST.Global
import LLVM.AST.AddrSpace
import LLVM.AST.FunctionAttribute
import LLVM.AST.Type

{-# ANN module ("HLint: ignore Unnecessary hiding" :: String) #-}

data ExternalDefinition = ExternalDefinition Definition CallableOperand


headerType, layoutType :: Type
layoutType = StructureType False [i16, i16, i32]
headerType = StructureType False [ptr headerTypeRef, ptr headerTypeRef, layoutType]
headerTypeRef :: Type
headerTypeRef = NamedTypeReference $ Name "__heap_object"
headerTypeDef :: Definition
headerTypeDef = TypeDefinition (Name "__heap_object") $ Just headerType

newAllocate :: Global
newAllocate = functionDefaults
  { name = Name "__allocate"
  , parameters = ([Parameter i64 (Name "num_bytes") []], False)
  , returnType = ptr headerTypeRef
  }


printf :: Global
printf = functionDefaults
  { name = Name "printf"
  , parameters = ([Parameter (ptr i8) (Name "fmt") []], True)
  , returnType = i32
  }


allocate :: Global
allocate = functionDefaults
  { name = Name "__tl_allocate"
  , parameters = ([Parameter i64 (Name "bytes") []], False)
  , returnType = ptr i8
  }


-- | static __heap_object* __create_int(int32_t value)
createInt :: Global
createInt = functionDefaults
  { name = Name "__create_int"
  , parameters = ([Parameter i32 (Name "value") []], False)
  , returnType = ptr i32
  }


-- | void* __get_object_slot(__heap_object* obj, uint16_t index)
getObjectSlot :: Global
getObjectSlot = functionDefaults
  { name = Name "__get_object_slot"
  , parameters = ([Parameter (ptr i32) (Name "obj") [], Parameter (i16) (Name "index") []], False)
  , returnType = ptr i32
  }


pushScope :: Global
pushScope = functionDefaults
  { name = Name "__push_scope"
  , parameters = ([Parameter (ptr i32) (Name "loc") []], False)
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
  , parameters = ([Parameter i32 (Name "exit_code") []], False)
  , returnType = VoidType
  , functionAttributes = [Right NoReturn, Right NoUnwind]
  }
