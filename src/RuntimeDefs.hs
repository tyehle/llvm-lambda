{-# LANGUAGE FlexibleContexts, NamedFieldPuns, OverloadedStrings, TupleSections #-}

module RuntimeDefs where

import qualified Control.Monad as M ( void, zipWithM_ )

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

data Runtime = Runtime
  { printf       :: Operand
  , exit         :: Operand
  -- | runtime.c defs
  , layout   :: Type
  , header       :: Type
  , scopeCell    :: Type
  , allObjects   :: Operand
  , numObjects   :: Operand
  , inScope      :: Operand
  , allocate     :: Operand
  , runGC        :: Operand
  , markObjects  :: Operand
  , printObject  :: Operand
  , printScope   :: Operand
  -- | Local defs
  , pushScopeDef :: Operand
  , popScopeDef  :: Operand
  , setSlotDef   :: Operand
  , getSlotDef   :: Operand
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


defineRuntime :: MonadModuleBuilder m => m Runtime
defineRuntime = do
  printf <- externVarArgs "printf" [ptr i8] i32
  exit <- extern "exit" [i32] void
  malloc <- extern "malloc" [i32] (ptr i8)
  free <- extern "free" [ptr i8] void

  layout <- defineLayout
  header <- defineHeader layout
  scopeCell <- defineScopeCell header
  allObjects <- externGlobal "__all_objects" (ptr header)
  numObjects <- externGlobal "__num_objects" i64
  inScope <- globalNull "__in_scope" scopeCell
  allocate <- extern "__allocate" [i64] (ptr header)
  runGC <- extern "__run_gc" [] void
  markObjects <- extern "__mark_heap_objects" [ptr header] void
  printObject <- extern "__print_object" [ptr header] void
  printScope <- extern "__print_scope" [] void
  pushScope <- definePushScope malloc header scopeCell inScope
  popScope <- definePopScope scopeCell inScope printf exit free
  setSlot <- defineSetSlot header
  getSlot <- defineGetSlot header

  return $ Runtime
    { printf = printf
    , exit = exit
    , layout = layout
    , header = header
    , scopeCell = scopeCell
    , allObjects = allObjects
    , numObjects = numObjects
    , inScope = inScope
    , allocate = allocate
    , runGC = runGC
    , markObjects = markObjects
    , printObject = printObject
    , printScope = printScope
    , pushScopeDef = pushScope
    , popScopeDef = popScope
    , setSlotDef = setSlot
    , getSlotDef = getSlot
    }

defineLayout :: MonadModuleBuilder m => m Type
defineLayout = typedef "__layout" $ Just $ StructureType False [i16, i16, i16, i16]

defineHeader :: MonadModuleBuilder m => Type -> m Type
defineHeader layout = typedef name $ Just $ StructureType False [ptr ref, layout]
  where
    name = "__heap_object"
    ref = NamedTypeReference name

defineScopeCell :: MonadModuleBuilder m => Type -> m Type
defineScopeCell header = typedef name $ Just $ StructureType False [ptr header, ptr ref]
  where
    name = "__scope_cell"
    ref = NamedTypeReference name

definePushScope :: MonadModuleBuilder m => Operand -> Type -> Type -> Operand -> m Operand
definePushScope malloc header scopeCell inScope = function "__push_scope" [(ptr header, "obj")] void pushScope
  where
    pushScope [obj] = do
      oldHead <- load inScope 8
      cell <- call malloc [(int32 16, [])] >>= flip bitcast (ptr scopeCell)
      cellObj <- gep cell [int64 0, int32 0]
      store cellObj 8 obj
      cellPrev <- gep cell [int64 0, int32 1]
      store cellPrev 8 oldHead
      store inScope 8 cell
      retVoid

pushScope :: MonadIRBuilder m => Runtime -> Operand -> m ()
pushScope Runtime{pushScopeDef} obj = M.void $ call pushScopeDef [(obj, [])]

definePopScope :: MonadModuleBuilder m
  => Type    -- ^ scopeCell struct type
  -> Operand -- ^ inScope pointer to list of in scope cells
  -> Operand -- ^ printf
  -> Operand -- ^ exit
  -> Operand -- ^ free
  -> m Operand
definePopScope scopeCell inScope printf exit free = function "__pop_scope" [] void popScope
  where
    popScope [] = do
      err <- freshName "error"
      ok <- freshName "ok"
      -- if(inScope == NULL)
      oldHead <- load inScope 8
      cond <- icmp L.EQ oldHead (ConstantOperand $ C.Null (ptr scopeCell))
      condBr cond err ok

      -- error block
      emitBlockStart err
      errString <- globalStringPtr "Runtime Error: No objects in scope to remove!\n" "pop_scope_error_msg"
      call printf [(ConstantOperand errString, [])]
      call exit [(int32 (-1), [])]
      unreachable

      -- ok block
      emitBlockStart ok
      -- inScope = inScope->prev
      -- free(oldHead)
      prevPtr <- gep oldHead [int64 0, int32 1]
      prev <- load prevPtr 8
      store inScope 8 prev
      oldHeadI8 <- bitcast oldHead $ ptr i8
      call free [(oldHeadI8, [])]
      retVoid

popScope :: MonadIRBuilder m => Runtime -> m ()
popScope Runtime{popScopeDef} = M.void $ call popScopeDef []

defineSetSlot :: MonadModuleBuilder m => Type -> m Operand
defineSetSlot header = function "__set_object_slot" [(ptr header, "obj"), (i64, "index"), (ptr header, "value")] void body
  where
    body [obj, index, value] = do
      objSucc <- gep obj [int64 1]
      values <- bitcast objSucc (ptr $ ptr header)
      slot <- gep values [index]
      store slot 8 value
      retVoid

setSlot :: MonadIRBuilder m => Runtime -> Operand -> Operand -> Operand -> m Operand
setSlot Runtime{setSlotDef} obj index value = call setSlotDef [(obj, []), (index, []), (value, [])]

defineGetSlot :: MonadModuleBuilder m => Type -> m Operand
defineGetSlot header = function "__get_object_slot" [(ptr header, "obj"), (i64, "index")] (ptr header) body
  where
    body [obj, index] = getSlot obj index >>= ret
    getSlot :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Operand -> m Operand
    getSlot obj index = do
      objSucc <- gep obj [int64 1]
      values <- bitcast objSucc (ptr $ ptr header)
      slot <- gep values [index]
      load slot 8

getSlot :: MonadIRBuilder m => Runtime -> Operand -> Operand -> m Operand
getSlot Runtime{getSlotDef} obj index = call getSlotDef [(obj, []), (index, [])]


-- | Object creation routines

createInt :: (MonadIRBuilder m, MonadModuleBuilder m) => Runtime -> Operand -> m Operand
createInt runtime@Runtime{header, allocate} value = do
  obj <- call allocate [(int64 8, [])]
  ptrValue <- inttoptr value (ptr header)
  setSlot runtime obj (int64 0) ptrValue
  return obj

getInt :: (MonadIRBuilder m, MonadModuleBuilder m) => Runtime -> Operand -> m Operand
getInt runtime obj = getSlot runtime obj (int64 0) >>= flip ptrtoint i64

createClosure :: (MonadIRBuilder m, MonadModuleBuilder m)
  => Runtime     -- ^ Runtime
  -> Operand     -- ^ Function pointer
  -> [Operand]   -- ^ Pointers
  -> [Operand]   -- ^ Values
  -> m Operand
createClosure Runtime{allocate} fn pointers values = do
  let size = fromIntegral $ 8 * (length pointers + length values + 1)
  obj <- call allocate [(int64 size, [])]
  -- set num pointers
  numPointersAddr <- gep obj [int64 0, int32 1, int32 2]
  store numPointersAddr 8 (int16 $ fromIntegral $ length pointers)
  -- set slots
  array <- gep obj [int64 1] >>= flip bitcast (ptr $ ptr i8)
  M.zipWithM_ (storePtr array) [0..] pointers
  M.zipWithM_ (storeValue array) [fromIntegral $ length pointers..] values
  _ <- storePtr array (fromIntegral $ length pointers + length values) fn
  return obj
  where
    storePtr array index value = bitcast value (ptr i8) >>= storeArray array index

    storeValue array index value = inttoptr value (ptr i8) >>= storeArray array index

    storeArray array index value = do
      addr <- gep array [int64 index]
      store addr 8 value

callClosure :: (MonadIRBuilder m, MonadModuleBuilder m)
  => Runtime -- ^ Runtime
  -> Operand     -- ^ Closure
  -> [Operand]   -- ^ Arguments
  -> m Operand
callClosure runtime@Runtime{header} closure args = do
  envSize <- objectSize closure
  fnSlot <- sub envSize (int16 1) >>= flip zext i64
  let numArgs = 1 + length args
  let fnType = ptr $ FunctionType (ptr header) (replicate numArgs $ ptr header) False
  fnPtr <- getSlot runtime closure fnSlot >>= flip bitcast fnType
  call fnPtr $ (closure, []) : map (,[]) args
  where
    objectSize obj = do
      loc <- gep obj [int64 0, int32 1, int32 1]
      load loc 2
