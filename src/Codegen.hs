module Codegen where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BS (toShort)

import LLVM.AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module hiding (Module)

import Expr

generate :: Expr -> IO ByteString
generate = undefined

doubleType :: Type
doubleType = FloatingPointType DoubleFP

intType :: Type
intType = IntegerType 64

emptyModule :: String -> Module
emptyModule label = defaultModule { moduleName = BS.toShort . BS.pack $ label }
