{-# LANGUAGE FlexibleContexts, NamedFieldPuns, OverloadedStrings, TupleSections #-}

module RuntimeDefs where

import LLVM.AST ( Definition(GlobalDefinition), Name, Operand)
import qualified LLVM.AST.IntegerPredicate as L (IntegerPredicate(EQ))
-- import LLVM.AST.DataLayout
-- import LLVM.AST.FunctionAttribute
-- import qualified LLVM.AST.IntegerPredicate as IPred
import LLVM.AST.Global
import qualified LLVM.AST.Constant as C
-- import LLVM.AST.AddrSpace (AddrSpace(AddrSpace))
-- import LLVM.AST.Linkage (Linkage(Private))
import LLVM.AST.Type
-- import LLVM.Context
-- import LLVM.Module hiding (Module)
import LLVM.AST.Operand ( Operand(ConstantOperand) )

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant


-- import Fresh

data RuntimeRefs = RuntimeRefs
  { printf      :: Operand
  , exit        :: Operand
  -- | runtime.c defs
  , layoutType  :: Type
  , headerType  :: Type
  , allObjects  :: Operand
  , numObjects  :: Operand
  , inScope     :: Operand
  , allocate    :: Operand
  , runGC       :: Operand
  , markObjects :: Operand
  , printObject :: Operand
  -- | Local defs
  , pushScope   :: Operand
  , popScope    :: Operand
  , setSlot     :: Operand
  , getSlot     :: Operand
  } deriving (Eq, Ord, Show)


globalNull :: MonadModuleBuilder m => Name -> Type -> m Operand
globalNull name ty = global name (ptr ty) $ C.Null (ptr ty)

externGlobal :: MonadModuleBuilder m => Name -> Type -> m Operand
externGlobal name ty = do
  emitDefn $ GlobalDefinition globalVariableDefaults
    { name=name
    , type'=ty
    }
  return $ ConstantOperand $ C.GlobalReference (ptr ty) name

int16 :: Integer -> Operand
int16  = ConstantOperand . C.Int 16


defineRuntime :: MonadModuleBuilder m => m RuntimeRefs
defineRuntime = do
  printf <- externVarArgs "printf" [ptr i8] i32
  exit <- extern "exit" [i32] void

  layoutType <- defineLayout
  headerType <- defineHeader layoutType
  allObjects <- externGlobal "__all_objects" (ptr headerType)
  numObjects <- externGlobal "__num_objects" i64
  inScope <- globalNull "__in_scope" headerType
  allocate <- extern "__allocate" [i64] (ptr headerType)
  runGC <- extern "__run_gc" [] void
  markObjects <- extern "__mark_heap_objects" [ptr headerType] void
  printObject <- extern "__print_object" [ptr i8] void
  pushScope <- definePushScope headerType inScope
  popScope <- definePopScope headerType inScope printf exit
  setSlot <- defineSetSlot headerType
  getSlot <- defineGetSlot headerType

  return $ RuntimeRefs
    { printf = printf
    , exit = exit
    , layoutType = layoutType
    , headerType = headerType
    , allObjects = allObjects
    , numObjects = numObjects
    , inScope = inScope
    , allocate = allocate
    , runGC = runGC
    , markObjects = markObjects
    , printObject = printObject
    , pushScope = pushScope
    , popScope = popScope
    , setSlot = setSlot
    , getSlot = getSlot
    }

defineLayout :: MonadModuleBuilder m => m Type
defineLayout = typedef "__layout" $ Just $ StructureType False [i16, i16, i16, i16]

defineHeader :: MonadModuleBuilder m => Type -> m Type
defineHeader layoutType = typedef name $ Just $ StructureType False [ptr ref, ptr ref, layoutType]
  where
    name = "__heap_object"
    ref = NamedTypeReference name

definePushScope :: MonadModuleBuilder m => Type -> Operand -> m Operand
definePushScope header inScope = function "__push_scope" [(ptr header, "obj")] void pushScope
  where
    pushScope [obj] = do
      newLink <- gep obj [int64 0, int32 1]
      oldHead <- load inScope 8
      store newLink 8 oldHead
      store inScope 8 obj
      retVoid

definePopScope :: MonadModuleBuilder m => Type -> Operand -> Operand -> Operand -> m Operand
definePopScope header inScope printf exit = function "__pop_scope" [] void popScope
  where
    popScope [] = do
      err <- freshName "error"
      ok <- freshName "ok"
      -- if(inScope == NULL)
      oldHead <- load inScope 8
      cond <- icmp L.EQ oldHead (ConstantOperand $ C.Null (ptr header))
      condBr cond err ok

      -- error block
      emitBlockStart err
      errString <- globalStringPtr "Runtime Error: No objects in scope to remove!\n" "pop_scope_error_msg"
      call printf [(ConstantOperand errString, [])]
      call exit [(int32 (-1), [])]
      unreachable

      -- ok block
      emitBlockStart ok
      -- inScope = inScope->scopeLink
      prevPtr <- gep oldHead [int64 0, int32 1]
      prev <- load prevPtr 8
      store inScope 8 prev
      retVoid

defineSetSlot :: MonadModuleBuilder m => Type -> m Operand
defineSetSlot header = function "__set_object_slot" [(ptr header, "obj"), (i64, "index"), (ptr i8, "value")] void body
  where
    body [obj, index, value] = do
      objSucc <- gep obj [int64 1]
      values <- bitcast objSucc (ptr $ ptr i8)
      slot <- gep values [index]
      store slot 8 value
      retVoid

defineGetSlot :: MonadModuleBuilder m => Type -> m Operand
defineGetSlot header = function "__get_object_slot" [(ptr header, "obj"), (i64, "index")] (ptr i8) body
  where
    body [obj, index] = getSlot obj index >>= ret
    getSlot :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Operand -> m Operand
    getSlot obj index = do
      objSucc <- gep obj [int64 1]
      values <- bitcast objSucc (ptr $ ptr i8)
      slot <- gep values [index]
      load slot 8


-- | Object creation routines

createInt :: (MonadIRBuilder m, MonadModuleBuilder m) => RuntimeRefs -> Integer -> m Operand
createInt RuntimeRefs{allocate, setSlot} value = do
  obj <- call allocate [(int64 8, [])]
  ptrValue <- inttoptr (int64 value) (ptr i8)
  call setSlot [(obj, []), (int64 0, []), (ptrValue, [])]
  return obj

getInt :: (MonadIRBuilder m, MonadModuleBuilder m) => RuntimeRefs -> Operand -> m Operand
getInt RuntimeRefs{getSlot} obj = call getSlot [(obj, []), (int64 0, [])]

createClosure :: (MonadIRBuilder m, MonadModuleBuilder m)
  => RuntimeRefs -- ^ Runtime
  -> Operand     -- ^ Function pointer
  -> [Operand]   -- ^ Pointers
  -> [Operand]   -- ^ Values
  -> m Operand
createClosure RuntimeRefs{allocate} fn pointers values = do
  let size = fromIntegral $ 8 * (length pointers + length values + 1)
  obj <- call allocate [(int64 size, [])]
  -- set num pointers
  numPointersAddr <- gep obj [int64 0, int32 2, int32 2]
  store numPointersAddr 8 (int16 $ fromIntegral $ length pointers)
  -- set slots
  array <- gep obj [int64 1] >>= flip bitcast (ptr $ ptr i8)
  _ <- sequence $ zipWith (storePtr array) [0..] pointers
  _ <- sequence $ zipWith (storeValue array) [fromIntegral $ length pointers..] values
  _ <- storePtr array (fromIntegral $ length pointers + length values) fn
  return obj
  where
    storePtr array index value = bitcast value (ptr i8) >>= storeArray array index

    storeValue array index value = inttoptr value (ptr i8) >>= storeArray array index

    storeArray array index value = do
      addr <- gep array [int64 index]
      store addr 8 value

callClosure :: (MonadIRBuilder m, MonadModuleBuilder m)
  => RuntimeRefs -- ^ Runtime
  -> Operand     -- ^ Closure
  -> [Operand]   -- ^ Arguments
  -> m Operand
callClosure RuntimeRefs{headerType, getSlot} closure args = do
  envSize <- objectSize closure
  fnSlot <- sub envSize (int16 1) >>= flip zext i64
  let fnType = ptr $ FunctionType (ptr i8) (ptr headerType : map (const (ptr i8)) args) False
  fnPtr <- call getSlot [(closure, []), (fnSlot, [])] >>= flip bitcast fnType
  call fnPtr $ (closure, []) : map (,[]) args
  where
    objectSize obj = do
      loc <- gep obj [int64 0, int32 2, int32 1]
      load loc 2
