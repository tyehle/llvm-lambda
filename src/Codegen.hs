{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Codegen where

import Data.Char (ord)
import Data.ByteString (ByteString)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict
import Control.Monad.Identity
import Data.Word (Word32)

import LLVM.AST hiding (value, index, expected, args)
import LLVM.AST.DataLayout
import LLVM.AST.FunctionAttribute
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import LLVM.AST.AddrSpace (AddrSpace(AddrSpace))
import LLVM.AST.Linkage (Linkage(Private))
import LLVM.Context
import LLVM.Module hiding (Module)

import ANorm
import Fresh
import LibCDefs

generate :: Prog -> Fresh (IO ByteString)
generate prog = do
  llvmModule <- genModule prog
  return $ withContext $ \ctx -> withModuleFromAST ctx llvmModule moduleLLVMAssembly


-- sample :: IO ByteString
-- sample = withContext $ \ctx -> withModuleFromAST ctx ir moduleLLVMAssembly
--   where
--     ir :: Module
--     ir = let globals = map (GlobalDefinition . snd) . Map.toList . defs . execFreshCodegen $ buildModule
--            in defaultModule { moduleName = "main", moduleDefinitions = globals }
--
--     addDefs :: FreshCodegen ()
--     addDefs = do
--       defineFmtString
--       defineGuard
--       defineCheckArity
--       finishFunction malloc
--       finishFunction exit
--       finishFunction printf
--
--     buildFunction :: FreshCodegen Name
--     buildFunction = do
--       let envParam = (star $ star $ IntegerType 32, Name "env")
--       let argParam = (star $ IntegerType 32, Name "x")
--
--       -- get a refernece to the environment
--       let env = uncurry LocalReference envParam
--
--       -- get a value from the environment
--       valueBox <- getFromEnv env 0
--       value <- getNum valueBox
--
--       arg <- getNum (uncurry LocalReference argParam)
--       added <- doInstruction (IntegerType 32) $ Add False False value arg []
--       result <- allocNumber added
--
--       finishBlock (Name "entry") (Do $ Ret (Just result) [])
--
--       let functionDef = defParams
--             { G.name = Name "increment"
--             , G.parameters = ([uncurry Parameter envParam [], uncurry Parameter argParam []], False)
--             }
--       finishFunction functionDef
--
--       return $ G.name functionDef
--
--     buildModule :: FreshCodegen ()
--     buildModule = do
--       addDefs
--
--       loc <- allocNumber . ConstantOperand . C.Int 32 $ 319
--
--       funcName <- inNewScope buildFunction
--
--       clos <- allocClosure funcName [loc]
--
--       result <- callClosure clos [loc]
--       printOperand result
--
--       finishBlock (Name "entry") (Do $ Ret Nothing [])
--       finishFunction functionDefaults
--         { G.name = Name "main"
--         , G.parameters = ([], False)
--         , G.returnType = VoidType
--         }


newtype CodegenT m a =
  CodegenT { codegenState :: StateT CodegenState m a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

type Env = Map String Operand

data CodegenState = CodegenState
  { defs :: Map Name Global
  , blockName :: Maybe Name
  , blocks :: [BasicBlock]
  , instructions :: [Named Instruction]
  , environment :: Env
  }

type Codegen = CodegenT Identity

modifyEnvironment :: MonadState CodegenState m => (Env -> Env) -> m ()
modifyEnvironment f = modify $ \s -> s { environment = f . environment $ s }

addInstruction :: MonadState CodegenState m => Named Instruction -> m ()
addInstruction instr = modify $ \s -> s { instructions = instr : instructions s }

type FreshCodegen = FreshT Codegen

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState { defs=Map.empty, blockName=Nothing, blocks=[], instructions=[], environment=Map.empty }

runCodegen :: FreshCodegen a -> Fresh CodegenState
runCodegen (FreshT (StateT f)) = FreshT $ StateT $ inner . f
  where
    inner :: Applicative m => Codegen (a, s) -> m (CodegenState, s)
    inner = pure . (\((_, s), res) -> (res, s)) . flip runState emptyCodegenState . codegenState


pointerBits :: Word32
pointerBits = fst $ pointerLayouts (defaultDataLayout LittleEndian) ! AddrSpace 0

closureTag, intTag :: Operand
closureTag = ConstantOperand $ C.Int 32 0
intTag     = ConstantOperand $ C.Int 32 1


defineGuard :: FreshCodegen ()
defineGuard = do
  let actualParam   = (star $ IntegerType 32, Name "actual")
  let expectedParam = (IntegerType 32, Name "expected")

  errBlock <- uniqueName "err"
  okBlock <- uniqueName "ok"

  uniqueName "entry" >>= startBlock
  actualValue <- doInstruction (IntegerType 32) $ Load True (uncurry LocalReference actualParam) Nothing 0 []
  notEq <- doInstruction (IntegerType 1) $ ICmp IPred.NE actualValue (uncurry LocalReference expectedParam) []
  finishBlock $ Do $ CondBr notEq errBlock okBlock []

  startBlock errBlock
  addInstruction $ Do $ callGlobal exit [ConstantOperand (C.Int 32 1)]
  finishBlock $ Do $ Unreachable []

  startBlock okBlock
  finishBlock $ Do $ Ret Nothing []

  defineFunction functionDefaults
    { G.name = Name "guard"
    , G.parameters = ([uncurry Parameter actualParam [], uncurry Parameter expectedParam []], False)
    , G.returnType = VoidType
    , G.functionAttributes = [Right InlineHint]
    }


defineCheckArity :: FreshCodegen ()
defineCheckArity = do
  let closure = (star $ IntegerType 32, Name "closure")
  let numArgs = (IntegerType 16, Name "numArgs")

  errBlock <- uniqueName "err"
  okBlock <- uniqueName "ok"

  uniqueName "entry" >>= startBlock
  shortArray <- cast (star $ IntegerType 16) $ uncurry LocalReference closure
  arityLoc <- doInstruction (star $ IntegerType 16) $ arrayAccess shortArray 2
  arity <- doInstruction (IntegerType 16) $ Load True arityLoc Nothing 0 []
  notEq <- doInstruction (IntegerType 1) $ ICmp IPred.NE arity (uncurry LocalReference numArgs) []
  finishBlock $ Do $ CondBr notEq errBlock okBlock []

  startBlock errBlock
  addInstruction $ Do $ callGlobal exit [ConstantOperand (C.Int 32 2)]
  finishBlock $ Do $ Unreachable []

  startBlock okBlock
  finishBlock $ Do $ Ret Nothing []

  defineFunction functionDefaults
    { G.name = Name "checkArity"
    , G.parameters = ([uncurry Parameter closure [], uncurry Parameter numArgs []], False)
    , G.returnType = VoidType
    , G.functionAttributes = [Right InlineHint]
    }


genModule :: Prog -> Fresh Module
genModule (Prog progDefs expr) = do { moduleDefs <- allDefs; return defaultModule { moduleName = "main", moduleDefinitions = moduleDefs } }
  where
    allDefs :: Fresh [Definition]
    allDefs = map (GlobalDefinition . snd) . Map.toList . defs <$> runCodegen buildModule

    declareDef :: Def -> FreshCodegen ()
    declareDef (ClosureDef name envName argNames _) =
      modify $ \s -> s { defs = Map.insert defName emptyDef (defs s) }
      where
        defName = mkName name
        argParam argName = Parameter (star $ IntegerType 32) (mkName argName) []
        params = Parameter (star (star $ IntegerType 32)) (mkName envName) [] : map argParam argNames
        emptyDef = defParams { G.name = defName , G.parameters = (params , False) }

    addGlobalDef :: Def -> FreshCodegen ()
    addGlobalDef (ClosureDef name envName argNames body) = do
      uniqueName "entry" >>= startBlock
      let env = LocalReference (star (star (IntegerType 32))) (mkName envName)
      let mkArg argName = LocalReference (star (IntegerType 32)) (mkName argName)

      -- mark all locals as in scope
      -- the start of the closure is 16 bytes before the start of the environment
      closureLoc <- doInstruction (star $ IntegerType 32) (arrayAccess env (-2)) >>= cast (star $ IntegerType 32)
      _ <- addInstruction $ Do $ callGlobal pushScope [closureLoc]
      mapM_ (\name -> addInstruction $ Do $ callGlobal pushScope [mkArg name]) argNames

      -- put all the arguments into the environment
      oldEnv <- gets environment
      modifyEnvironment $ Map.insert envName env
      let insertions = zipWith Map.insert argNames (map mkArg argNames)
      mapM_ modifyEnvironment insertions

      -- generate the function code
      result <- synthFunctionBody (length argNames + 1) body
      -- reset the environment
      modifyEnvironment $ const oldEnv
      finishFunction $ mkName name

    buildModule :: FreshCodegen ()
    buildModule = do
      addDefs
      -- Run codegen for definitions
      mapM_ declareDef progDefs
      mapM_ addGlobalDef progDefs

      -- Run codegen for main
      uniqueName "entry" >>= startBlock
      result <- synthExpr 0 expr

      -- Print result
      printOperand result

      finishBlock $ Do $ Ret (Just $ ConstantOperand $ C.Int 32 0) []
      defineFunction functionDefaults
        { G.name = Name "main"
        , G.parameters = ([], False)
        , G.returnType = IntegerType 32
        }

    addDefs :: FreshCodegen ()
    addDefs = do
      defineFmtString
      defineGuard
      defineCheckArity
      defineFunction allocate
      defineFunction pushScope
      defineFunction popScope
      defineFunction exit
      defineFunction printf


defineFmtString :: FreshCodegen ()
defineFmtString = modify $ \s -> s { defs = Map.insert name global (defs s) }
  where
    name = Name "fmt"
    global = globalVariableDefaults
      { G.name = name
      , G.linkage = Private
      , G.isConstant = True
      , G.type' = ArrayType 4 $ IntegerType 8
      , G.initializer = Just . C.Array (IntegerType 8) . mkString $ "%d\n\NUL"
      }
    mkString = map (C.Int 8 . fromIntegral . ord)


printOperand :: Operand -> FreshCodegen ()
printOperand loc = do
  value <- getNum loc
  let fmtRef = C.GlobalReference (PointerType (ArrayType 4 $ IntegerType 8) (AddrSpace 0)) (Name "fmt")
  let fmtArg = ConstantOperand $ C.GetElementPtr True fmtRef [C.Int 32 0, C.Int 32 0]
  addInstruction $ Do $ callGlobal printf [fmtArg, value]
  return ()


defParams :: Global
defParams = functionDefaults { G.returnType = star (IntegerType 32) }


cast :: Type -> Operand -> FreshCodegen Operand
cast typ input = doInstruction typ $ BitCast input typ []


arrayAccess :: Operand -> Integer -> Instruction
arrayAccess array index = GetElementPtr True array [ConstantOperand $ C.Int pointerBits index] []


--         i32*                     i32
getNum :: Operand -> FreshCodegen Operand
getNum loc = do
  checkTag loc intTag
  valueLoc <- doInstruction (star $ IntegerType 32) $ GetElementPtr True loc [ConstantOperand (C.Int pointerBits 1)] []
  doInstruction (IntegerType 32) $ Load True valueLoc Nothing 0 []


allocNumber :: Operand -> FreshCodegen Operand
allocNumber num = do
  -- malloc a number
  mem <- doInstruction (star $ IntegerType 8) $ callGlobal allocate [ConstantOperand $ C.Int 64 8]
  loc <- cast (star $ IntegerType 32) mem
  -- set the type tag
  addInstruction $ Do $ Store True loc intTag Nothing 0 []
  -- set the value
  valueLoc <- doInstruction (star $ IntegerType 32) $ arrayAccess loc 1
  addInstruction $ Do $ Store True valueLoc num Nothing 0 []
  -- return the memory location
  return loc


checkTag :: Operand -> Operand -> FreshCodegen ()
checkTag actual expected = do
  guardDef <- findGuard
  addInstruction $ Do $ callGlobal guardDef [actual, expected]
  where
    findGuard = gets $ fromMaybe (error "no guard definition found") . Map.lookup (Name "guard") . defs


checkArity :: Operand -> Integer -> FreshCodegen ()
checkArity clos numArgs = do
  def <- findDef
  addInstruction $ Do $ callGlobal def [clos, ConstantOperand (C.Int 16 numArgs)]
  where
    findDef = gets $ fromMaybe (error "no checkArity definition found") . Map.lookup (Name "checkArity") . defs


callClosure :: Int -> Bool -> Operand -> [Operand] -> FreshCodegen Operand
callClosure requiredPops isTail clos args = do
  -- make sure we've got a closure
  checkTag clos closureTag
  let arity = length args
  checkArity clos (fromIntegral arity)
  -- find where the pointer array starts
  array <- doInstruction (IntegerType 32) (arrayAccess clos 2) >>= cast (star (star (IntegerType 32)))
  -- get the function
  let functionType = FunctionType (star $ IntegerType 32) (star (star $ IntegerType 32) : replicate arity (star $ IntegerType 32)) False
  funcPtr <- doInstruction (star $ IntegerType 32) (arrayAccess array 0) >>= cast (star $ star functionType)
  func <- doInstruction (star functionType) $ Load True funcPtr Nothing 0 []
  -- get the environment
  env <- doInstruction (star $ star $ IntegerType 32) $ arrayAccess array 1
  -- pop the local scope
  callPopScope requiredPops
  -- call the function
  let cc = G.callingConvention defParams
      retAttrs = G.returnAttributes defParams
      fAttrs = G.functionAttributes defParams
      tailCallKind = if isTail then Just Tail else Just NoTail
  doInstruction (star $ IntegerType 32) $ Call tailCallKind cc retAttrs (Right func) (zip (env:args) $ repeat []) fAttrs []


allocClosure :: Name -> [Operand] -> FreshCodegen Operand
allocClosure funcName values = do
  -- tag: i32, arity: i16, size: i16, funcPointer: ptr, envValues: ptr ...
  -- need enough memory for two ints and then an array of pointers
  -- all sizes are in bytes
  let pointerBytes = fromIntegral pointerBits `quot` 8
  let closureSize = ConstantOperand . C.Int 64 . fromIntegral $ 8 + pointerBytes * (length values + 1)
  loc <- doInstruction (star $ IntegerType 8) (callGlobal allocate [closureSize]) >>= cast (star $ IntegerType 32)
  -- set the type tag
  addInstruction $ Do $ Store True loc closureTag Nothing 0 []
  -- set number of args
  loc16 <- cast (star $ IntegerType 16) loc
  arityLoc <- doInstruction (star $ IntegerType 16) $ arrayAccess loc16 2
  arity <- defArity
  addInstruction $ Do $ Store True arityLoc (ConstantOperand (C.Int 16 arity)) Nothing 0 []
  -- set the size of the environment
  sizeLoc <- doInstruction (star $ IntegerType 16) $ arrayAccess loc16 3
  let size = fromIntegral $ length values
  addInstruction $ Do $ Store True sizeLoc (ConstantOperand (C.Int 16 size)) Nothing 0 []
  -- get a reference to the funcPtr
  let argTypes = star (star $ IntegerType 32) : replicate (fromIntegral arity) (star $ IntegerType 32)
  let funcRef = C.GlobalReference (star $ FunctionType (star $ IntegerType 32) argTypes False) funcName
  funcPtr <- cast (star $ IntegerType 32) $ ConstantOperand funcRef
  -- fill in the array of pointers
  arrayLoc <- doInstruction (star $ IntegerType 32) (arrayAccess loc 2) >>= cast (star $ star $ IntegerType 32)
  storePtr arrayLoc (funcPtr, 0)
  -- fill in the values in the array
  mapM_ (storePtr arrayLoc) (zip values [1..])
  return loc
  where
    defArity :: FreshCodegen Integer
    defArity = do
      global <- gets $ \s -> fromMaybe (error ("Function not defined: " ++ show funcName)) $ Map.lookup funcName (defs s)
      return . subtract 1 . fromIntegral . length . fst . G.parameters $ global
    storePtr :: Operand -> (Operand, Integer) -> FreshCodegen ()
    storePtr arrayLoc (ptr, index) = do
      wherePut <- doInstruction (star $ star $ IntegerType 32) $ arrayAccess arrayLoc index
      addInstruction $ Do $ Store True wherePut ptr Nothing 0 []


callPopScope :: Int -> FreshCodegen ()
callPopScope n = replicateM_ n $ addInstruction $ Do $ callGlobal popScope []


getFromEnv :: Operand -> Integer -> FreshCodegen Operand
getFromEnv env index = do
  loc <- doInstruction (star $ star $ IntegerType 32) $ arrayAccess env index
  doInstruction (star $ IntegerType 32) $ Load True loc Nothing 0 []


type IntBinOp = Bool -> Bool -> Operand -> Operand -> InstructionMetadata -> Instruction

numBinOp :: AExpr -> AExpr -> IntBinOp -> FreshCodegen Operand
numBinOp a b op = do
  aValue <- synthAExpr a >>= getNum
  bValue <- synthAExpr b >>= getNum
  resultValue <- doInstruction (IntegerType 32) $ op False False aValue bValue []
  allocNumber resultValue


synthIf0 :: Int -> AExpr -> Expr -> Expr -> FreshCodegen Operand
synthIf0 requiredPops c t f = do
  condInt <- synthAExpr c >>= getNum
  condValue <- doInstruction (IntegerType 1) $ ICmp IPred.EQ condInt (ConstantOperand $ C.Int 32 0) []
  trueLabel <- uniqueName "trueBlock"
  falseLabel <- uniqueName "falseBlock"
  exitLabel <- uniqueName "ifExit"
  finishBlock $ Do $  CondBr condValue trueLabel falseLabel []
  -- Define the true block
  startBlock trueLabel
  trueValue <- synthExpr requiredPops t
  finishBlock $ Do $ Br exitLabel []
  -- Define the false block
  startBlock falseLabel
  falseValue <- synthExpr requiredPops f
  finishBlock $ Do $ Br exitLabel []
  -- get the correct output
  startBlock exitLabel
  doInstruction (star $ IntegerType 32) $ Phi (star $ IntegerType 32) [(trueValue, trueLabel), (falseValue, falseLabel)] []


synthFunctionBody :: Int -> Expr -> FreshCodegen ()
synthFunctionBody requiredPops (If0 c t f) = do
  condInt <- synthAExpr c >>= getNum
  condValue <- doInstruction (IntegerType 1) $ ICmp IPred.EQ condInt (ConstantOperand $ C.Int 32 0) []
  trueLabel <- uniqueName "trueBlock"
  falseLabel <- uniqueName "falseBlock"
  finishBlock $ Do $  CondBr condValue trueLabel falseLabel []
  -- true block
  startBlock trueLabel
  synthFunctionBody requiredPops t
  -- false block
  startBlock falseLabel
  synthFunctionBody requiredPops f

synthFunctionBody requiredPops (Let name binding body) = do
  oldEnv <- gets environment
  bindingName <- synthExpr 0 binding
  _ <- addInstruction $ Do $ callGlobal pushScope [bindingName]
  modifyEnvironment $ Map.insert name bindingName
  _ <- synthFunctionBody (requiredPops + 1) body
  modifyEnvironment $ const oldEnv

synthFunctionBody requiredPops (App funcName argExprs) = do
  -- find the function in the list of definitions
  global <- gets $ \s -> fromMaybe (error ("Function not defined: " ++ funcName)) $ Map.lookup (mkName funcName) (defs s)
  -- find all our arguments
  args <- mapM synthAExpr argExprs
  -- pop our local scope before we make the call
  callPopScope requiredPops
  result <- doInstruction (star $ IntegerType 32) $ callGlobal global args
  finishBlock $ Do $ Ret (Just result) []

synthFunctionBody requiredPops (AppClos closExpr argExprs) = do
  clos <- synthAExpr closExpr
  args <- mapM synthAExpr argExprs
  result <- callClosure requiredPops True clos args
  finishBlock $ Do $ Ret (Just result) []

synthFunctionBody requiredPops other = synthExpr requiredPops other >>= finishBlock . Do . flip Ret [] . Just


synthAExpr :: AExpr -> FreshCodegen Operand
synthAExpr (Ref name) = do
  env <- gets environment
  return . fromMaybe (error ("Unbound name: " ++ name)) $ Map.lookup name env

synthAExpr (GetEnv envName index) = do
  env <- synthAExpr (Ref envName)
  getFromEnv env index


synthExpr :: Int -> Expr -> FreshCodegen Operand
synthExpr requiredPops (Num n) = (allocNumber . ConstantOperand . C.Int 32 . fromIntegral $ n) <* callPopScope requiredPops

synthExpr requiredPops (Plus a b) = numBinOp a b Add <* callPopScope requiredPops

synthExpr requiredPops (Minus a b) = numBinOp a b Sub <* callPopScope requiredPops

synthExpr requiredPops (Mult a b) = numBinOp a b Mul <* callPopScope requiredPops

synthExpr requiredPops (Divide a b) = numBinOp a b (const SDiv) <* callPopScope requiredPops

synthExpr requiredPops (If0 c t f) = synthIf0 requiredPops c t f

synthExpr requiredPops (Let name binding body) = do
  oldEnv <- gets environment
  bindingName <- synthExpr 0 binding
  _ <- addInstruction $ Do $ callGlobal pushScope [bindingName]
  modifyEnvironment $ Map.insert name bindingName
  resultName <- synthExpr (requiredPops + 1) body
  modifyEnvironment $ const oldEnv
  return resultName

synthExpr requiredPops (App funcName argExprs) = do
  -- find the function in the list of definitions
  global <- gets $ \s -> fromMaybe (error ("Function not defined: " ++ funcName)) $ Map.lookup (mkName funcName) (defs s)
  -- find all our arguments
  args <- mapM synthAExpr argExprs
  -- pop our local scope before we make the call
  callPopScope requiredPops
  -- do the function call
  doInstruction (star $ IntegerType 32) $ callGlobal global args

synthExpr requiredPops (AppClos closExpr argExprs) = do
  clos <- synthAExpr closExpr
  args <- mapM synthAExpr argExprs
  callClosure requiredPops False clos args

synthExpr requiredPops (NewClos closName bindings) = do
  args <- mapM synthAExpr bindings
  allocClosure (mkName closName) args <* callPopScope requiredPops

synthExpr requiredPops (Atomic a) = synthAExpr a <* callPopScope requiredPops


doInstruction :: Type -> Instruction -> FreshCodegen Operand
doInstruction typ instr = do
  myName <- fresh
  addInstruction $ myName := instr
  return $ LocalReference typ myName


startBlock :: Name -> FreshCodegen ()
startBlock name = do
  currentName <- gets blockName
  case currentName of
    Nothing -> modify $ \s -> s { blockName = Just name }
    (Just n) -> error $ "Cant start block " ++ show name ++ "! Already working on block " ++ show n


finishBlock :: Named Terminator -> FreshCodegen ()
finishBlock term = do
  name <- gets $ fromMaybe (error "No block started!") . blockName
  instrs <- gets $ reverse . instructions
  let block = BasicBlock name instrs term
  modify $ \s -> s { blockName = Nothing, blocks = block : blocks s, instructions = [] }


defineFunction :: Global -> FreshCodegen ()
defineFunction emptyDef = do
  definedBlocks <- gets $ reverse . blocks
  let def = emptyDef { G.basicBlocks = definedBlocks }
  modify $ \s -> s { blocks = [], defs = Map.insert (G.name def) def (defs s) }


finishFunction :: Name -> FreshCodegen ()
finishFunction name = do
  emptyDef <- gets $ fromMaybe (error ("No definition to finish: " ++ show name)) . Map.lookup name . defs
  defineFunction emptyDef


inNewScope :: FreshCodegen a -> FreshCodegen a
inNewScope dirtyOps = do
  oldBlocks <- gets blocks
  oldInstructions <- gets instructions
  modify $ \s -> s { blocks = [], instructions = [] }
  result <- dirtyOps
  modify $ \s -> s { blocks = oldBlocks, instructions = oldInstructions }
  return result


callGlobal :: Global -> [Operand] -> Instruction
callGlobal func argOps = Call Nothing cc retAttrs fOp args fAttrs []
  where
    cc = G.callingConvention func
    retAttrs = G.returnAttributes func
    fAttrs = G.functionAttributes func
    args = map (flip (,) []) argOps
    fOp = Right $ ConstantOperand $ C.GlobalReference ptrType (G.name func)
    ptrType = PointerType (FunctionType retType argTypes varArgs) (AddrSpace 0)
    retType = G.returnType func
    argTypes = map typeOf . fst . G.parameters $ func
    typeOf (Parameter typ _ _) = typ
    varArgs = snd . G.parameters $ func
