{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Codegen where

import Data.Char (ord)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BS (toShort)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad.State
import Control.Monad.Identity
import Data.Word (Word32)

import LLVM.AST
import LLVM.AST.DataLayout
import LLVM.AST.FunctionAttribute
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import LLVM.AST.AddrSpace (AddrSpace(AddrSpace))
import LLVM.AST.CallingConvention (CallingConvention(C))
import LLVM.AST.Linkage (Linkage(Private))
import LLVM.Context
import LLVM.Module hiding (Module)

import LowLevel hiding (callClosure)
import Fresh
import LibCDefs
import Scope

generate :: Prog -> IO ByteString
generate prog = withContext $ \ctx -> withModuleFromAST ctx (genModule prog) moduleLLVMAssembly


sample :: IO ByteString
sample = withContext $ \ctx -> withModuleFromAST ctx ir moduleLLVMAssembly
  where
    ir :: Module
    ir = let globals = map (GlobalDefinition . snd) . Map.toList . defs . execFreshCodegen $ buildModule
           in defaultModule { moduleName = "main", moduleDefinitions = globals }

    addDefs :: FreshCodegen ()
    addDefs = do
      defineFmtString
      defineGuard
      defineCheckArity
      finishFunction malloc
      finishFunction exit
      finishFunction printf

    allocClosure :: Integer -> [Operand] -> FreshCodegen Operand
    allocClosure arity values = do
      -- need enough memory for two ints and then an array of pointers
      -- all sizes are in bytes
      let pointerBytes = fromIntegral pointerBits `quot` 8
      let closureSize = ConstantOperand . C.Int 64 . fromIntegral $ 8 + pointerBytes * length values
      loc <- doInstruction (star charType) (callGlobal malloc [closureSize]) >>= cast (star intType)
      -- set the type tag
      addInstruction $ Do $ Store True loc closureTag Nothing 0 []
      -- set number of args
      arityLoc <- doInstruction (star intType) $ arrayAccess loc 1
      addInstruction $ Do $ Store True arityLoc (ConstantOperand (C.Int 32 arity)) Nothing 0 []
      -- get a reference to the array
      arrayLoc <- doInstruction (star intType) (arrayAccess loc 2) >>= cast (star (star intType))
      -- fill in the values in the array
      mapM_ (storePtr arrayLoc) (zip values [0..])

      return loc

      where
        storePtr :: Operand -> (Operand, Integer) -> FreshCodegen ()
        storePtr arrayLoc (ptr, index) = do
          wherePut <- doInstruction (star (star intType)) $ arrayAccess arrayLoc index
          addInstruction $ Do $ Store True wherePut ptr Nothing 0 []

    buildFunction :: FreshCodegen Global
    buildFunction = do
      let envParam = (star (star intType), Name "env")
      let argParam = (star intType, Name "x")

      -- get a refernece to the environment
      let env = uncurry LocalReference envParam

      -- get a value from the environment
      valueBox <- getFromEnv env 0
      value <- getNum valueBox

      arg <- getNum (uncurry LocalReference argParam)
      added <- doInstruction intType $ Add False False value arg []
      result <- allocNumber added

      finishBlock (Name "entry") (Do $ Ret (Just result) [])

      let functionDef = lambdaDefaults
            { G.name = Name "increment"
            , G.parameters = ([uncurry Parameter envParam [], uncurry Parameter argParam []], False)
            }
      finishFunction functionDef
      return functionDef

      where
        getFromEnv :: Operand -> Integer -> FreshCodegen Operand
        getFromEnv env index = do
          loc <- doInstruction (star (star intType)) $ arrayAccess env 0
          doInstruction (star intType) $ Load True loc Nothing 0 []

    lambdaDefaults :: Global
    lambdaDefaults = functionDefaults { G.returnType = star intType }

    callClosure :: Operand -> [Operand] -> FreshCodegen Operand
    callClosure clos args = do
      -- make sure we've got a closure
      checkTag clos closureTag
      let arity = length args
      checkArity clos (fromIntegral arity)
      -- find where the pointer array starts
      array <- doInstruction intType (arrayAccess clos 2) >>= cast (star (star intType))
      -- get the function
      let functionType = FunctionType (star intType) (star (star intType) : replicate arity (star intType)) False
      funcPtr <- doInstruction (star intType) (arrayAccess array 0) >>= cast (star (star functionType))
      func <- doInstruction (star functionType) $ Load True funcPtr Nothing 0 []
      -- get the environment
      env <- doInstruction (star (star intType)) $ arrayAccess array 1
      -- call the function
      let cc = G.callingConvention lambdaDefaults
          retAttrs = G.returnAttributes lambdaDefaults
          fAttrs = G.functionAttributes lambdaDefaults
      doInstruction (star intType) $ Call Nothing cc retAttrs (Right func) (zip (env:args) $ repeat []) fAttrs []

    buildModule :: FreshCodegen ()
    buildModule = do
      addDefs

      loc <- allocNumber . ConstantOperand . C.Int 32 $ 319

      funcDef <- inNewScope buildFunction

      let funcRef = C.GlobalReference (star (FunctionType (star intType) [star (star intType), star intType] False)) (G.name funcDef)
      funcPtr <- cast (star intType) $ ConstantOperand funcRef

      clos <- allocClosure 1 [funcPtr, loc]

      result <- callClosure clos [loc]
      printOperand result

      finishBlock (Name "entry") (Do $ Ret Nothing [])
      finishFunction functionDefaults
        { G.name = Name "main"
        , G.parameters = ([], False)
        , G.returnType = VoidType
        }


newtype CodegenT m a =
  CodegenT { codegenState :: StateT CodegenState m a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

type Env = Map String Operand

data CodegenState = CodegenState
  { defs :: Map Name Global
  , blocks :: [BasicBlock]
  , instructions :: [Named Instruction]
  , environment :: Env
  }

type Codegen = CodegenT Identity

execCodegen :: Codegen a -> CodegenState -> CodegenState
execCodegen = execState . codegenState

modifyEnvironment :: MonadState CodegenState m => (Env -> Env) -> m ()
modifyEnvironment f = modify $ \s -> s { environment = f . environment $ s }

addInstruction :: MonadState CodegenState m => Named Instruction -> m ()
addInstruction instr = modify $ \s -> s { instructions = instr : instructions s }

type FreshCodegen = FreshT Codegen

execFreshCodegen :: FreshCodegen a -> CodegenState
execFreshCodegen = flip execCodegen emptyState . flip evalFreshT Map.empty
  where
    emptyState = CodegenState { defs=Map.empty, blocks=[], instructions=[], environment=Map.empty }


pointerBits :: Word32
pointerBits = fst $ pointerLayouts (defaultDataLayout LittleEndian) ! AddrSpace 0

closureTag, intTag :: Operand
closureTag = ConstantOperand $ C.Int 32 0
intTag     = ConstantOperand $ C.Int 32 1


defineGuard :: FreshCodegen ()
defineGuard = do
  let actualParam   = (star intType, Name "actual")
  let expectedParam = (intType, Name "expected")

  actualValue <- doInstruction intType $ Load True (uncurry LocalReference actualParam) Nothing 0 []
  notEq <- doInstruction boolType $ ICmp IPred.NE actualValue (uncurry LocalReference expectedParam) []
  finishBlock (Name "entry") $ Do $ CondBr notEq (Name "err") (Name "ok") []

  addInstruction $ Do $ callGlobal exit [ConstantOperand (C.Int 32 1)]
  finishBlock (Name "err") $ Do $ Unreachable []

  finishBlock (Name "ok") $ Do $ Ret Nothing []

  finishFunction functionDefaults
    { G.name = Name "guard"
    , G.parameters = ([uncurry Parameter actualParam [], uncurry Parameter expectedParam []], False)
    , G.returnType = VoidType
    , G.functionAttributes = [Right InlineHint]
    }


defineCheckArity :: FreshCodegen ()
defineCheckArity = do
  let closure = (star intType, Name "closure")
  let numArgs = (intType, Name "numArgs")

  arityLoc <- doInstruction (star intType) (arrayAccess (uncurry LocalReference closure) 1)
  arity <- doInstruction intType $ Load True arityLoc Nothing 0 []
  notEq <- doInstruction boolType $ ICmp IPred.NE arity (uncurry LocalReference numArgs) []
  finishBlock (Name "entry") $ Do $ CondBr notEq (Name "err") (Name "ok") []

  addInstruction $ Do $ callGlobal exit [ConstantOperand (C.Int 32 2)]
  finishBlock (Name "err") $ Do $ Unreachable []

  finishBlock (Name "ok") $ Do $ Ret Nothing []

  finishFunction functionDefaults
    { G.name = Name "checkArity"
    , G.parameters = ([uncurry Parameter closure [], uncurry Parameter numArgs []], False)
    , G.returnType = VoidType
    , G.functionAttributes = [Right InlineHint]
    }


genModule :: Prog -> Module
genModule (Prog progDefs expr) = defaultModule { moduleName = "main", moduleDefinitions = allDefs }
  where
    allDefs :: [Definition]
    allDefs = map (GlobalDefinition . snd) . Map.toList . defs . execFreshCodegen $ buildModule

    buildModule :: FreshCodegen ()
    buildModule = do
      addDefs
      -- Run codegen for definitions

      -- Run codegen for main
      result <- synthExpr expr

      -- Print result

      -- TODO: Make sure that synthExpr actually does allocation
      loc <- allocNumber result

      printOperand loc

      finishBlock (Name "entry") (Do $ Ret Nothing [])
      finishFunction functionDefaults
        { G.name = Name "main"
        , G.parameters = ([], False)
        , G.returnType = VoidType
        }

    addDefs :: FreshCodegen ()
    addDefs = do
      defineFmtString
      defineGuard
      defineCheckArity
      finishFunction malloc
      finishFunction exit
      finishFunction printf

    numberFmt :: Definition
    numberFmt = GlobalDefinition globalVariableDefaults
      { G.name = Name "fmt"
      , G.linkage = Private
      , G.isConstant = True
      , G.type' = ArrayType 4 charType
      , G.initializer = Just $ C.Array charType [C.Int 8 37, C.Int 8 100, C.Int 8 10, C.Int 8 0]
      }

    body :: [Named Instruction]
    body = reverse . instructions . execFreshCodegen $ synthExpr expr >>= printResult
    printResult :: Operand -> FreshCodegen ()
    printResult reg = do
      let refType = PointerType (ArrayType 4 charType) (AddrSpace 0)
      let fmtArg = ConstantOperand $ C.GetElementPtr True (C.GlobalReference refType (Name "fmt")) [C.Int 32 0, C.Int 32 0]
      addInstruction $ Do $ callGlobal printf [fmtArg, reg]


defineFmtString :: FreshCodegen ()
defineFmtString = modify $ \s -> s { defs = Map.insert name global (defs s) }
  where
    name = Name "fmt"
    global = globalVariableDefaults
      { G.name = name
      , G.linkage = Private
      , G.isConstant = True
      , G.type' = ArrayType 4 charType
      , G.initializer = Just . C.Array charType . mkString $ "%d\n\NUL"
      }
    mkString = map (C.Int 8 . fromIntegral . ord)


printOperand :: Operand -> FreshCodegen ()
printOperand loc = do
  value <- getNum loc
  let fmtRef = C.GlobalReference (PointerType (ArrayType 4 charType) (AddrSpace 0)) (Name "fmt")
  let fmtArg = ConstantOperand $ C.GetElementPtr True fmtRef [C.Int 32 0, C.Int 32 0]
  addInstruction $ Do $ callGlobal printf [fmtArg, value]
  return ()


synthDef :: Def -> FreshCodegen Global
synthDef (Def name argNames body) = undefined


cast :: Type -> Operand -> FreshCodegen Operand
cast typ input = doInstruction typ $ BitCast input typ []


arrayAccess :: Operand -> Integer -> Instruction
arrayAccess array index = GetElementPtr True array [ConstantOperand $ C.Int pointerBits index] []


--         i32*                     i32
getNum :: Operand -> FreshCodegen Operand
getNum loc = do
  checkTag loc intTag
  valueLoc <- doInstruction (star intType) $ GetElementPtr True loc [ConstantOperand (C.Int pointerBits 1)] []
  doInstruction intType $ Load True valueLoc Nothing 0 []


allocNumber :: Operand -> FreshCodegen Operand
allocNumber num = do
  -- malloc a number
  mem <- doInstruction (star charType) $ callGlobal malloc [ConstantOperand $ C.Int 64 8]
  loc <- cast (star intType) mem
  -- set the type tag
  addInstruction $ Do $ Store True loc intTag Nothing 0 []
  -- set the value
  valueLoc <- doInstruction (star intType) $ arrayAccess loc 1
  addInstruction $ Do $ Store True valueLoc num Nothing 0 []
  -- return the memory location
  return loc


checkTag :: Operand -> Operand -> FreshCodegen ()
checkTag actual expected = do
  guardDef <- findGuard
  addInstruction $ Do $ callGlobal guardDef [actual, expected]
  where
    findGuard = gets $ fromMaybe (error "no guard definition found") . Map.lookup (Name "guard") . defs
    matches name def = G.name def == name


checkArity :: Operand -> Integer -> FreshCodegen ()
checkArity clos numArgs = do
  def <- findDef
  addInstruction $ Do $ callGlobal def [clos, ConstantOperand (C.Int 32 numArgs)]
  where
    findDef = gets $ fromMaybe (error "no checkArity definition found") . Map.lookup (Name "checkArity") . defs


synthExpr :: Expr -> FreshCodegen Operand
synthExpr (Num n) = return . ConstantOperand . C.Int 32 . fromIntegral $ n
synthExpr (Plus a b) = do
  aName <- synthExpr a
  bName <- synthExpr b
  doInstruction intType $ Add False False aName bName []
synthExpr (Minus a b) = do
  aName <- synthExpr a
  bName <- synthExpr b
  doInstruction intType $ Sub False False aName bName []
synthExpr (Let name binding body) = do
  oldEnv <- gets environment
  bindingName <- synthExpr binding
  modifyEnvironment $ Map.insert name bindingName
  resultName <- synthExpr body
  modifyEnvironment $ const oldEnv
  return resultName
synthExpr (Ref name) = do
  env <- gets environment
  return $ env ! name
synthExpr (App funcName args) = undefined
synthExpr (AppClos clos args) = undefined
synthExpr (NewClos closName size) = undefined
synthExpr (SetEnv clos index binding body) = undefined
synthExpr (GetEnv clos index) = undefined


doInstruction :: Type -> Instruction -> FreshCodegen Operand
doInstruction typ instr = do
  myName <- fresh
  addInstruction $ myName := instr
  return $ LocalReference typ myName


finishBlock :: Name -> Named Terminator -> FreshCodegen ()
finishBlock name term = do
  instrs <- gets $ reverse . instructions
  let block = BasicBlock name instrs term
  modify $ \s -> s { blocks = block : blocks s, instructions = [] }


finishFunction :: Global -> FreshCodegen ()
finishFunction defaults = do
  blocks <- gets $ reverse . blocks
  let def = defaults { G.basicBlocks = blocks }
  modify $ \s -> s { blocks = [], defs = Map.insert (G.name def) def (defs s) }


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
