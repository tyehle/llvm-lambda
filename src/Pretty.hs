{-# LANGUAGE FlexibleContexts #-}
module Pretty where

import Control.Monad.Except
import Data.List (intercalate)
import Data.Char (ord, chr)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import HighLevel
import ParseLisp
import qualified LowLevel as LL
import qualified ANorm as A
import Types
import Scope

subscript :: Int -> String
subscript = map conv . show
  where
    conv c = chr $ ord c + 8272


class Pretty a where
  pretty :: a -> String


-- | Add some additional context to an error message
contextualize :: (Pretty c, MonadError String m) => c -> m a -> m a
contextualize context comp = catchError comp (\err -> throwError $ err ++ "\n  in " ++ pretty context)


instance Pretty Lisp where
  pretty expr = case expr of
    Symbol s -> s
    List exprs -> "(" ++ unwords (map pretty exprs) ++ ")"
    String s -> show s
    Number n -> show n
    Float f -> show f


instance Pretty Prog where
  pretty (Prog defs exprs) = unlines $ map pretty defs ++ map pretty exprs


instance Pretty Def where
  pretty (StructDef typeRef constructors) = "(struct " ++ unwords (pretty typeRef : map go constructors) ++ ")"
    where go (ConsIdent name, args) = "[" ++ unwords (name : map pretty args) ++ "]"


instance Pretty TypeRef where
  pretty (TypeRef (TypeIdent name) []) = name
  pretty (TypeRef (TypeIdent name) args) = "(" ++ unwords (name : map (\(TypeIdent s) -> s) args) ++ ")"


instance Pretty CasePattern where
  pretty (VarBinding (VarIdent name)) = name
  pretty (ConsPattern (ConsIdent ident) subPatterns) = "(" ++ unwords (ident : map pretty subPatterns) ++ ")"


instance Pretty Expr where
  pretty expr = case expr of
    Ref name -> unwrap name
    Nat n -> show n
    Lambda args body -> "(λ (" ++ unwords (map unwrap args) ++ ") " ++ pretty body ++ ")"
    Let name value body -> "(let [" ++ unwrap name ++ " " ++ pretty value ++ "] " ++ pretty body ++ ")"
    Letrec name value body -> "(letrec [" ++ unwrap name ++ " " ++ pretty value ++ "] " ++ pretty body ++ ")"
    Case obj patterns -> "(case " ++ unwords (pretty obj : map prettyCaseClause patterns) ++ ")"
    App fn args -> "(" ++ pretty fn ++ " " ++ unwords (map pretty args) ++ ")"
    If0 c t f -> "(if0 " ++ pretty c ++ " " ++ pretty t ++ " " ++ pretty f ++ ")"
    BinOp op a b -> "(" ++ pretty op ++ " " ++ pretty a ++ " " ++ pretty b ++ ")"
    where
      unwrap (VarIdent name) = name
      prettyCaseClause (pattern, body) = "[" ++ pretty pattern ++ " " ++ pretty body ++ "]"


instance Pretty BinOp where
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mul = "*"
  pretty Div = "/"
  -- pretty Mod = "%"


instance Pretty MonoType where
  pretty monoType = case renumber $ PolyType [] monoType of
    PolyType (_:_) _ -> "Error renumbering type variables"
    PolyType [] (TVar (TVarIdent n)) -> "τ" ++ subscript n
    PolyType [] (TLam argTypes retType) -> "(" ++ intercalate ", " (map pretty argTypes) ++ ") → " ++ pretty retType
    PolyType [] (TApp name types) -> unwords $ name : map pretty types


instance Pretty PolyType where
  pretty polyType = case renumber polyType of
    (PolyType idents monoType) -> "∀ " ++ unwords (map prettyIdent idents) ++ ". " ++ pretty monoType
    where
      prettyIdent (TVarIdent ident) = "τ" ++ subscript ident


-- | Renumbers all the type identifiers to make them easier to read
renumber :: PolyType -> PolyType
renumber (PolyType idents monoType) = PolyType (map (identMap !) idents) $ go identMap monoType
  where
    free = Set.toList $ freeVars monoType `Set.difference` Set.fromList idents
    identMap = Map.fromList $ zip (free ++ idents) (map TVarIdent [1..])
    go :: Map TVarIdent TVarIdent -> MonoType -> MonoType
    go identMap (TVar ident) = TVar $ identMap ! ident
    go identMap (TLam args res) = TLam (map (go identMap) args) (go identMap res)
    go identMap (TApp name args) = TApp name $ map (go identMap) args


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
