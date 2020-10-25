{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Expr where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Scope

newtype VarIdent = VarIdent String deriving (Eq, Ord, Show)

data Expr = Nat Int
          | BinOp BinOp Expr Expr
          | If0 Expr Expr Expr
          | Ref VarIdent
          | Let VarIdent Expr Expr
          | Letrec VarIdent Expr Expr
          | Lambda [VarIdent] Expr
          | App Expr [Expr]
          deriving (Eq, Ord, Show)


data BinOp
  = Add
  | Sub
  | Mul
  | Div
  --  | Mod
  deriving (Eq, Ord, Show)


instance Scope Expr VarIdent where
  freeVars (Nat _) = Set.empty
  freeVars (BinOp _ a b) = freeVars a `Set.union` freeVars b
  freeVars (If0 c t f) = Set.unions [freeVars c, freeVars t, freeVars f]
  freeVars (Ref name) = Set.singleton name
  freeVars (Let name binding body) = freeVars binding `Set.union` Set.delete name (freeVars body)
  freeVars (Letrec name binding body) = Set.delete name $ freeVars binding `Set.union` freeVars body
  freeVars (Lambda args body) = freeVars body `Set.difference` Set.fromList args
  freeVars (App fn args) = Set.unions $ map freeVars (fn:args)

instance Substitute Expr where
  substitute trySub expr = case trySub expr of
    Just ex -> ex
    Nothing -> case expr of
      Nat _ -> expr
      BinOp op a b -> BinOp op (recur a) (recur b)
      If0 c t f -> If0 (recur c) (recur t) (recur f)
      Ref _ -> expr
      Let name binding body -> Let name (recur binding) (recur body)
      Letrec name binding body -> Letrec name (recur binding) (recur body)
      Lambda args body -> Lambda args (recur body)
      App fn args -> App (recur fn) (map recur args)
    where
      recur = substitute trySub


replaceRefs :: Map.Map VarIdent Expr -> Expr -> Expr
replaceRefs initialSubs = substitute $ go initialSubs
  where
    go :: Map.Map VarIdent Expr -> Expr -> Maybe Expr
    go subs (Ref name) = substitute (go $ Map.delete name subs) <$> Map.lookup name subs
    go subs (Let name value body) = Just $ Let name (substitute (go subs) value) (substitute (go $ Map.delete name subs) body)
    go subs (Letrec name value body) = let subs' = Map.delete name subs in Just $ Letrec name (substitute (go subs') value) (substitute (go subs') body)
    go subs (Lambda args body) = Just $ Lambda args $ substitute (go $ foldr Map.delete subs args) body
    go _ _ = Nothing
