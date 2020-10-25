module Pretty where

import Data.List (intercalate)
import Data.Char (ord, chr)

import Expr
import qualified LowLevel as LL
import Types

subscript :: Int -> String
subscript = map conv . show
  where
    conv c = chr $ ord c + 8272


class Pretty a where
  pretty :: a -> String

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
    LL.NewClos fName values  -> "(clos "++fName++" ["++intercalate " " (map pretty values)++"])"
    LL.GetEnv _ index  -> "(get-env "++show index++")"
    where
      prettyOp op args = "(" ++ intercalate " " (op : map pretty args) ++ ")"

instance Pretty LL.Def where
  pretty (LL.ClosureDef name _ argNames body) = "(closure-body ("++intercalate " " (name:argNames)++")\n  "++pretty body++")"

instance Pretty LL.Prog where
  pretty (LL.Prog defs expr) = "(prog\n"++defBlock++"\n  "++pretty expr++")\n"
    where
      defBlock = unlines $ map (\line -> "  " ++ line) $ lines $ intercalate "\n" (map pretty defs)
