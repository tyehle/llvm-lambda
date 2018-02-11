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

import LowLevel
import Fresh
import LibCDefs
import Scope

generate :: Prog -> IO ByteString
generate prog = withContext $ \ctx -> withModuleFromAST ctx (genModule prog) moduleLLVMAssembly


sample :: IO ByteString
sample = withContext $ \ctx -> withModuleFromAST ctx ir moduleLLVMAssembly
  where
    ir :: Module
    ir = let globals = map GlobalDefinition . defs . execFreshCodegen $ goFast
           in defaultModule { moduleName = "main", moduleDefinitions = globals }

    goFast :: FreshCodegen ()
    goFast = do
      defineFmtString
      defineGuard
      finishFunction malloc
      finishFunction exit
      finishFunction printf
      let count   = ConstantOperand $ C.Int 64 2
      let ptrsize = ConstantOperand $ C.Int 64 (fromIntegral pointerBits `quot` 8)
      let tagSize = ConstantOperand $ C.Int 64 4

      -- -- malloc a closure
      -- envSize <- doInstruction longType $ Mul False False ptrsize count []
      -- size    <- doInstruction longType $ Add False False envSize tagSize []
      -- loc <- doInstruction (star charType) $ callExternal malloc [size]
      --
      -- -- set the type tag
      -- locType <- doInstruction (star intType) $ BitCast loc (star intType) []
      -- addInstruction $ Do $ Store True locType closureTag Nothing 1 []

      -- malloc a number
      mem <- doInstruction (star charType) $ callGlobal malloc [ConstantOperand $ C.Int 64 8]
      loc <- doInstruction (star intType) $ BitCast mem (star intType) []

      -- set the type tag
      addInstruction $ Do $ Store True loc intTag Nothing 0 []

      -- set the value
      valueLoc <- doInstruction (star intType) $ GetElementPtr True loc [ConstantOperand (C.Int pointerBits 1)] []
      addInstruction $ Do $ Store True valueLoc (ConstantOperand (C.Int 32 33)) Nothing 0 []

      -- get a reference to the array of pointers
      -- refsLoc <- doInstruction (star (star intType)) $ Add False False loc tagSize []
      -- set the function pointer
      -- put things in the environment

      printOperand loc

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
  { defs :: [Global]
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
    emptyState = CodegenState { defs=[], blocks=[], instructions=[], environment=Map.empty }


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
    body = instructions . execFreshCodegen $ synthExpr expr >>= printResult

    printResult :: Operand -> FreshCodegen ()
    printResult reg = do
      let refType = PointerType (ArrayType 4 charType) (AddrSpace 0)
      let fmtArg = ConstantOperand $ C.GetElementPtr True (C.GlobalReference refType (Name "fmt")) [C.Int 32 0, C.Int 32 0]
      addInstruction $ Do $ callGlobal printf [fmtArg, reg]


defineFmtString :: FreshCodegen ()
defineFmtString = modify $ \s -> s { defs = global : defs s }
  where
    global = globalVariableDefaults
      { G.name = Name "fmt"
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
    findGuard = gets $ fromMaybe (error "no gurad definition found") . listToMaybe . filter ((Name "guard" ==) . G.name) . defs
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
  modify $ \s -> s { blocks = [], defs = def : defs s }


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
