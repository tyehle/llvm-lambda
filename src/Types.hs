{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import qualified Data.Set as Set

import Scope

newtype TVarIdent = TVarIdent Int deriving (Show, Eq, Ord)

data MonoType
  = TVar TVarIdent
  | TLam [MonoType] MonoType
  | TApp String [MonoType]
  deriving (Show, Eq, Ord)


data PolyType
  = PolyType [TVarIdent] MonoType
  deriving (Show, Eq, Ord)


instance Scope MonoType TVarIdent where
  freeVars monoType = case monoType of
    TVar ident -> Set.singleton ident
    TLam argTypes retType -> Set.unions $ map freeVars (retType:argTypes)
    TApp _ args -> Set.unions $ map freeVars args

instance Scope PolyType TVarIdent where
  freeVars (PolyType idents monoType) = foldr Set.delete (freeVars monoType) idents
