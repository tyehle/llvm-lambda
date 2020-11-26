{-# LANGUAGE FlexibleContexts #-}
module Pretty where

import Control.Monad.Except
import Data.List (intercalate)
import Data.Char (ord, chr)

import Expr
import qualified LowLevel as LL
import qualified ANorm as A
import Types

subscript :: Int -> String
subscript = map conv . show
  where
    conv c = chr $ ord c + 8272


class Pretty a where
  pretty :: a -> String



-- | Add some additional context to an error message
contextualize :: (Pretty c, MonadError String m) => c -> m a -> m a
contextualize context comp = catchError comp (\err -> throwError $ err ++ "\n  in " ++ pretty context)



instance Pretty Expr where
  pretty expr = case expr of
    Ref name -> unwrap name
    Nat n -> show n
    Lambda args body -> "(λ (" ++ unwords (map unwrap args) ++ ") " ++ pretty body ++ ")"
    Let name value body -> "(let [" ++ unwrap name ++ " " ++ pretty value ++ "] " ++ pretty body ++ ")"
    Letrec name value body -> "(letrec [" ++ unwrap name ++ " " ++ pretty value ++ "] " ++ pretty body ++ ")"
    App fn args -> "(" ++ pretty fn ++ " " ++ unwords (map pretty args) ++ ")"
    If0 c t f -> "(if0 " ++ pretty c ++ " " ++ pretty t ++ " " ++ pretty f ++ ")"
    BinOp op a b -> "(" ++ pretty op ++ " " ++ pretty a ++ " " ++ pretty b ++ ")"
    where
      unwrap (VarIdent name) = name

instance Pretty BinOp where
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mul = "*"
  pretty Div = "/"
  -- pretty Mod = "%"

instance Pretty MonoType where
  pretty monoType = case monoType of
    TVar (TVarIdent n) -> "τ" ++ subscript n
    TLam argTypes retType -> "(" ++ intercalate ", " (map pretty argTypes) ++ ") → " ++ pretty retType
    TApp name types -> unwords $ name : map pretty types

instance Pretty PolyType where
  pretty (PolyType idents monoType) = "∀ " ++ unwords (map prettyIdent idents) ++ ". " ++ pretty monoType
    where
      prettyIdent (TVarIdent ident) = "τ" ++ subscript ident


instance Pretty LL.Expr where
  pretty expr = case expr of
    LL.Num n                 -> show n
    LL.Plus a b              -> prettyOp "+" [a, b]
    LL.Minus a b             -> prettyOp "-" [a, b]
    LL.Mult a b              -> prettyOp "*" [a, b]
    LL.Divide a b            -> prettyOp "/" [a, b]
    LL.If0 c t f             -> prettyOp "if0" [c, t, f]
    LL.Let name value body   -> "(let ["++name++" "++pretty value++"] "++pretty body++")"
    LL.Ref name              -> name
    LL.App fName args        -> prettyOp fName args
    LL.AppClos fn args       -> prettyOp (pretty fn) args
    LL.NewClos fName values  -> "(clos "++fName++" ["++unwords (map pretty values)++"])"
    LL.GetEnv _ index  -> "(get-env "++show index++")"
    where
      prettyOp op args = "(" ++ unwords (op : map pretty args) ++ ")"

instance Pretty LL.Def where
  pretty (LL.ClosureDef name _ argNames body) = "(closure-body ("++unwords (name:argNames)++")\n  "++pretty body++")"

instance Pretty LL.Prog where
  pretty (LL.Prog defs expr) = "(prog\n"++defBlock++"\n  "++pretty expr++")\n"
    where
      defBlock = unlines $ map ("  " ++) $ lines $ intercalate "\n" (map pretty defs)


instance Pretty A.Expr where
  pretty expr = case expr of
    A.Num n                 -> show n
    A.BinOp op a b          -> prettyOp (pretty op) [a, b]
    A.If0 c t f             -> prettyOp "if0" [A.Atomic c, t, f]
    A.Let name value body   -> "(let ["++name++" "++pretty value++"] "++pretty body++")"
    A.App fName args        -> prettyOp fName args
    A.AppClos fn args       -> prettyOp (pretty fn) args
    A.NewClos fName values  -> "(clos "++fName++" ["++unwords (map pretty values)++"])"
    A.Atomic aExpr          -> pretty aExpr
    where
      prettyOp op args = "(" ++ unwords (op : map pretty args) ++ ")"

instance Pretty A.AExpr where
  pretty expr = case expr of
    A.Ref name -> name
    A.GetEnv _ index -> "(get-env "++show index++")"

instance Pretty A.BinOp where
  pretty op = case op of
    A.Add -> "+"
    A.Sub -> "-"
    A.Mul -> "*"
    A.Div -> "/"

instance Pretty A.Def where
  pretty (A.ClosureDef name _ argNames body) = "(closure-body ("++unwords (name:argNames)++")\n  "++pretty body++")"

instance Pretty A.Prog where
  pretty (A.Prog defs expr) = "(prog\n"++defBlock++"\n  "++pretty expr++")\n"
    where
      defBlock = unlines $ map ("  " ++) $ lines $ intercalate "\n" (map pretty defs)