{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Lib where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module hiding (Module)

data Expr = Nat Int
          | Plus Expr Expr
          | Minus Expr Expr
          | Ref String
          | Let String Expr Expr
          deriving (Eq, Show)

example :: Expr
example = Let "x" (Plus (Nat 1) (Nat 2)) $ Minus (Nat 5) (Ref "x")

someIR :: Module
someIR = defaultModule
  { moduleName=""
  , moduleDefinitions = [defAdd]
  }
  where
    int :: Type
    int = IntegerType 32
    defAdd :: Definition
    defAdd = GlobalDefinition functionDefaults
      { name = Name "add"
      , parameters = ([Parameter int (Name "a") [], Parameter int (Name "b") []], False)
      , returnType = int
      , basicBlocks = [body]
      }
    -- body :: Block
    body = BasicBlock
      (Name "entry")
      [Name "result" := Add
        False -- no signed wrap
        False -- no unsigned wrap
        (LocalReference int (Name "a"))
        (LocalReference int (Name "b"))
        []]
      (Do $ Ret (Just (LocalReference int (Name "result"))) [])

toLLVM :: Module -> IO ()
toLLVM ast = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx ast moduleLLVMAssembly
  BS.putStrLn llvm


class MonadFresh m where
  uniqueName :: String -> m String

fresh :: MonadFresh m => m String
fresh = uniqueName ""

instance Monad m => MonadFresh (StateT (Map String Int) m) where
  uniqueName name = do
    count <- gets $ fromMaybe 0 . Map.lookup name
    modify $ Map.insert name (count + 1)
    return $ name ++ show count
