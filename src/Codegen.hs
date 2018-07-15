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
      finishFunction malloc
      finishFunction exit
      finishFunction printf

    allocNumber :: Operand -> FreshCodegen Operand
    allocNumber num = do
      -- malloc a number
      mem <- doInstruction (star charType) $ callGlobal malloc [ConstantOperand $ C.Int 64 8]
      loc <- doInstruction (star intType) $ BitCast mem (star intType) []
      -- set the type tag
      addInstruction $ Do $ Store True loc intTag Nothing 0 []
      -- set the value
      valueLoc <- doInstruction (star intType) $ GetElementPtr True loc [ConstantOperand (C.Int pointerBits 1)] []
      addInstruction $ Do $ Store True valueLoc num Nothing 0 []
      -- return the memory location
      return loc

    allocClosure :: Integer -> [Operand] -> FreshCodegen Operand
    allocClosure arity values = do
      -- need enough memory for two ints and then an array of pointers
      -- all sizes are in bytes
      let pointerBytes = fromIntegral pointerBits `quot` 8
      let closureSize = ConstantOperand . C.Int 64 . fromIntegral $ 8 + pointerBytes * length values
      mem <- doInstruction (star charType) $ callGlobal malloc [closureSize]
      loc <- doInstruction (star intType) $ BitCast mem (star intType) []
      -- set the type tag
      addInstruction $ Do $ Store True loc closureTag Nothing 0 []
      -- set number of args
      arityLoc <- doInstruction (star intType) $ GetElementPtr True loc [ConstantOperand (C.Int pointerBits 1)] []
      addInstruction $ Do $ Store True arityLoc (ConstantOperand (C.Int 32 arity)) Nothing 0 []
      -- get a reference to the array
      arrayMem <- doInstruction (star intType) $ GetElementPtr True loc [ConstantOperand (C.Int pointerBits 2)] []
      arrayLoc <- doInstruction (star (star intType)) $ BitCast arrayMem (star (star intType)) []
      -- fill in the values in the array
      mapM_ (storePtr arrayLoc) (zip values [0..])

      return loc

      where
        storePtr :: Operand -> (Operand, Integer) -> FreshCodegen ()
        storePtr arrayLoc (ptr, index) = do
          wherePut <- doInstruction (star (star intType)) $ GetElementPtr True arrayLoc [ConstantOperand (C.Int pointerBits index)] []
          addInstruction $ Do $ Store True wherePut ptr Nothing 0 []

    buildFunction :: FreshCodegen Global
    buildFunction = do
      let envParam = (star (star intType), Name "env")
      let argParam = (star intType, Name "x")
      arg <- getNum (uncurry LocalReference argParam)
      value <- doInstruction intType $ Add False False (ConstantOperand (C.Int 32 1)) arg []
      result <- allocNumber value

      finishBlock (Name "entry") (Do $ Ret (Just result) [])

      let functionDef = lambdaDefaults
            { G.name = Name "increment"
            , G.parameters = ([uncurry Parameter envParam [], uncurry Parameter argParam []], False)
            }
      finishFunction functionDef
      return functionDef

    lambdaDefaults :: Global
    lambdaDefaults = functionDefaults { G.returnType = star intType }

    callClosure :: Operand -> [Operand] -> FreshCodegen Operand
    callClosure clos args = do
      -- make sure we've got a closure
      checkTag clos closureTag
      let arity = length args
      checkArity clos arity
      -- find where the pointer array starts
      arrayNoCast <- doInstruction intType $ GetElementPtr True clos [ConstantOperand $ C.Int pointerBits 2] []
      array <- doInstruction (star (star intType)) $ BitCast arrayNoCast (star (star intType)) []
      -- get the function
      let functionType = FunctionType (star intType) (star (star intType) : replicate arity (star intType)) False
      funcNoCast <- doInstruction (star intType) $ GetElementPtr True array [ConstantOperand $ C.Int pointerBits 0] []
      funcPtr <- doInstruction (star (star functionType)) $ BitCast funcNoCast (star (star functionType)) []
      func <- doInstruction (star functionType) $ Load True funcPtr Nothing 0 []
      -- get the environment
      env <- doInstruction (star (star intType)) $ GetElementPtr True array [ConstantOperand $ C.Int pointerBits 1] []
      -- call the function
      let cc = G.callingConvention lambdaDefaults
          retAttrs = G.returnAttributes lambdaDefaults
          fAttrs = G.functionAttributes lambdaDefaults
      doInstruction (star intType) $ Call Nothing cc retAttrs (Right func) (zip (env:args) $ repeat []) fAttrs []

      where
        checkArity :: Operand -> Int -> FreshCodegen ()
        checkArity clos arity = pure ()

    buildModule :: FreshCodegen ()
    buildModule = do
      addDefs

      loc <- allocNumber . ConstantOperand . C.Int 32 $ 381

      funcDef <- inNewScope buildFunction

      let funcRef = C.GlobalReference (star (FunctionType (star intType) [star (star intType), star intType] False)) (G.name funcDef)
      let funcPtr = ConstantOperand $ C.GetElementPtr True funcRef []-- C.Int 32 0]
      castFuncPtr <- doInstruction (star intType) $ BitCast funcPtr (star intType) []
      clos <- allocClosure 1 [castFuncPtr, loc] -- loc isn't used, but its good to test

      result <- callClosure clos [loc]
      printOperand result

      -- printOperand loc

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


buildMain :: [Named Instruction] -> Definition
buildMain body = GlobalDefinition functionDefaults
  { G.name = Name "main"
  , G.parameters = ([], False)
  , G.returnType = VoidType
  , G.basicBlocks = [BasicBlock (Name "entry") body (Do $ Ret Nothing [])]
  }


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


genModule :: Prog -> Module
genModule (Prog defs expr) = defaultModule { moduleName = "main", moduleDefinitions = numberFmt : buildMain body : externalDefs }
  where
    externalDefs :: [Definition]
    externalDefs = map GlobalDefinition [printf, malloc, free]

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


--         i32*                     i32
getNum :: Operand -> FreshCodegen Operand
getNum loc = do
  checkTag loc intTag
  valueLoc <- doInstruction (star intType) $ GetElementPtr True loc [ConstantOperand (C.Int pointerBits 1)] []
  doInstruction intType $ Load True valueLoc Nothing 0 []


checkTag :: Operand -> Operand -> FreshCodegen ()
checkTag actual expected = do
  guardDef <- findGuard
  addInstruction $ Do $ callGlobal guardDef [actual, expected]
  where
    findGuard = gets $ fromMaybe (error "no guard definition found") . Map.lookup (Name "guard") . defs
    matches name def = G.name def == name


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
synthExpr (NewClos closName) = undefined
synthExpr (SetEnv name binding clos body) = undefined
synthExpr (GetEnv name clos) = undefined


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
