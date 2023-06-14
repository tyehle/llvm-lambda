{-# LANGUAGE FlexibleContexts #-}
module Infer where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set

-- import Debug.Trace (trace)

import HighLevel
import Pretty
import Scope
import Types
import LLVM.AST (Type(resultType))

type TypeEnv = Map VarIdent PolyType
type ConsEnv = Map ConsIdent PolyType

type Subs = Map TVarIdent MonoType

data InferState = InferState TVarIdent (Map TVarIdent MonoType)
type Infer = ReaderT ConsEnv (State InferState)

type Fresh = State TVarIdent

-- | Create a fresh type variable
fresh :: Infer MonoType
fresh = do
  (InferState (TVarIdent n) s) <- get
  put $ InferState (TVarIdent (n + 1)) s
  return $ TVar $ TVarIdent n


-- | A version of zipWithM that throws an error if the lengths of the inputs don't match
zipWithMSafe :: MonadError String m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithMSafe fn [] [] = pure []
zipWithMSafe fn (a:as) (b:bs) = do
  c <- fn a b
  rest <- zipWithMSafe fn as bs
  return $ c : rest
zipWithMSafe _ _ _ = throwError "Type Error: Arity mismatch"


-- | lookup and instantiate the type of a constructor
lookupConstructorType :: ConsIdent -> ExceptT String Infer ([MonoType], MonoType)
lookupConstructorType name = do
  env <- ask
  monoType <- case Map.lookup name env of
    Nothing -> throwError $ "Undefined constructor: " ++ show name
    (Just polyType) -> lift $ instantiate polyType
  case monoType of
    (TLam args ret) -> return (args, ret)
    t@(TApp _ _) -> return ([], t)
    bad -> throwError $ "Constructor is not a function: " ++ show name ++ ": " ++ show bad


-- | Core types
numType :: MonoType
numType = TApp "Num" []


-- | Type of a BinOp as if it were a bound variable
binOpType :: BinOp -> PolyType
binOpType _ = numericBinOpType
  where
    numericBinOpType = PolyType [] (TLam [numType, numType] numType)


-- | Instantiate a polytype by creating fresh monotypes for all its arguments and subbing them in
instantiate :: PolyType -> Infer MonoType
instantiate (PolyType idents term) = do
  freshTVars <- mapM (const fresh) idents
  let subs = Map.fromList $ zip idents freshTVars
  return $ subMonoType subs term
  where
    subMonoType :: Subs -> MonoType -> MonoType
    subMonoType subs (TVar n) =
      case Map.lookup n subs of
        Nothing -> TVar n
        (Just t) -> subMonoType (Map.delete n subs) t -- delete from the sub so we can't hang
    subMonoType subs (TLam args ret) = TLam (map (subMonoType subs) args) (subMonoType subs ret)
    subMonoType subs (TApp name args) = TApp name $ map (subMonoType subs) args


-- | Make a polytype that quantifies all free type variables in a monotype that are not in the environment
generalize :: TypeEnv -> MonoType -> Infer PolyType
generalize env monoType = do
  resolved <- resolve monoType
  let tvars = Set.toList $ freeVars resolved `Set.difference` freeInEnv
  return $ PolyType tvars resolved
  where
    freeInEnv = Set.unions $ map freeVars (Map.elems env)


-- | Resolves all bound instances of type variables in a type
resolve :: MonoType -> Infer MonoType
resolve t = do
  (InferState _ subs) <- get
  return $ go subs t
  where
    go :: Subs -> MonoType -> MonoType
    go subs monoType = case monoType of
      TVar ident -> case Map.lookup ident subs of
        Nothing -> monoType
        (Just other) -> go (Map.delete ident subs) other -- Don't loop forever
      TLam args res -> TLam (map (go subs) args) (go subs res)
      TApp name args -> TApp name (map (go subs) args)


-- | Unify two types, possibly binding a type variable to another type
unify :: MonoType -> MonoType -> ExceptT String Infer ()
unify a b = do
  a' <- lift $ resolve a
  b' <- lift $ resolve b
  go a' b'
  where
    assign :: TVarIdent -> MonoType -> ExceptT String Infer ()
    assign ident monoType
      | monoType == TVar ident = return ()
      | occurs = throwError $ "Cannot create infinite type: " ++ pretty (TVar ident) ++ " = " ++ pretty monoType
      | otherwise = insert
      where
        occurs = Set.member ident $ freeVars monoType
        insert = do
          (InferState next subs) <- get
          let subs' = Map.insert ident monoType subs
          put $ InferState next subs'

    go :: MonoType -> MonoType -> ExceptT String Infer ()
    go (TVar ident) monoType = assign ident monoType
    go monoType (TVar ident) = assign ident monoType
    go (TLam argsA retA) (TLam argsB retB) = unifyAll (retA:argsA) (retB:argsB)
    go (TApp nameA argsA) (TApp nameB argsB) | nameA == nameB = unifyAll argsA argsB
    go badA badB = throwError $ "Type Error: Cannot unify " ++ pretty badA ++ " with " ++ pretty badB

-- | Unify two lists of types. If the lists are not the same size throw a type error
unifyAll :: [MonoType] -> [MonoType] -> ExceptT String Infer ()
unifyAll [] [] = return ()
unifyAll (a:as) (b:bs) = unify a b >> unifyAll as bs
unifyAll _ _ = throwError "Type Error: Arity mismatch"


-- | Infer the type of an expression
infer :: Prog -> Either String PolyType
infer (Prog defs [expr]) = runInference $ do
  constructors <- Map.fromList . concat <$> mapM constructorTypes defs
  local (const constructors) $ do
    exprType <- inferRec Map.empty expr
    lift $ generalize Map.empty exprType
  where
    runInference :: ExceptT String Infer a -> Either String a
    runInference comp = evalState (flip runReaderT Map.empty . runExceptT $ comp) $ InferState (TVarIdent 0) Map.empty

    structMap :: ExceptT String Infer ConsEnv
    structMap = Map.fromList . concat <$> mapM constructorTypes defs


-- | Create a list of constructor types for a struct definition
constructorTypes :: Def -> ExceptT String Infer [(ConsIdent, PolyType)]
constructorTypes def@(StructDef (TypeRef (TypeIdent name) tvars) constructors) = do
  tvarIdents <- mapM (const (lift fresh)) tvars
  let tvarsByName = Map.fromList $ zip tvars tvarIdents
      structType = TApp name tvarIdents
  contextualize def $ mapM (constructorType tvarsByName structType) constructors
  where
    lookupTVar :: TypeIdent -> Map TypeIdent MonoType -> ExceptT String Infer MonoType
    lookupTVar name varMap = case Map.lookup name varMap of
      Nothing -> throwError $ "Undefined type variable: " ++ show name
      (Just t) -> return t

    mkTApp :: Map TypeIdent MonoType -> TypeRef -> ExceptT String Infer MonoType
    mkTApp varMap (TypeRef ident []) | Map.member ident varMap = return $ varMap ! ident
    mkTApp varMap (TypeRef (TypeIdent name) tvars) = TApp name <$> mapM (flip lookupTVar varMap) tvars

    constructorType :: Map TypeIdent MonoType -> MonoType -> (ConsIdent, [TypeRef]) -> ExceptT String Infer (ConsIdent, PolyType)
    constructorType _ structType (name, []) = do
      generalStructType <- lift $ generalize Map.empty structType
      return (name, generalStructType)
    constructorType varMap structType (name, args) = do
      argTypes <- mapM (mkTApp varMap) args
      polyType <- lift . generalize Map.empty $ TLam argTypes structType
      return (name, polyType)


-- | Recursively infer the type of an expression with all the state needed to make that happen
inferRec :: TypeEnv -> Expr -> ExceptT String Infer MonoType
inferRec env expr = do
  (InferState _ s1) <- get
  -- trace ("\n>>> " ++ pretty expr ++ prettyState s1) $ return ()
  ret <- contextualize expr $ case expr of
    -- lookup s in the env and specialize if it so we can unify its quantified variables with real types
    Ref ident -> do
      -- allow constructors to be treated as regular variables
      constructors <- asks $ Map.mapKeys (\(ConsIdent name) -> VarIdent name)
      case Map.lookup ident (Map.union constructors env) of
        Nothing -> throwError $ "Undefined Variable: " ++ show ident
        (Just polyType) -> lift $ instantiate polyType

    Nat _ -> return numType

    -- typecheck the body with the argNames bound to new type vars in the environment
    Lambda argNames body -> do
      argTypes <- lift $ mapM (const fresh) argNames
      let bindings = Map.fromList $ zip argNames (map (PolyType []) argTypes)
      let env' = Map.union bindings env
      retType <- inferRec env' body
      return $ TLam argTypes retType

    -- infer the value type, generalize it, and then infer the body with the new type in the environment
    Let name value body -> do
      valueType <- inferRec env value
      polyType <- lift $ generalize env valueType
      let env' = Map.insert name polyType env
      inferRec env' body

    Letrec name value body -> do
      freshValueType <- lift fresh
      let valueEnv = Map.insert name (PolyType [] freshValueType) env
      valueType <- inferRec valueEnv value
      unify valueType freshValueType
      polyType <- lift $ generalize env valueType
      let bodyEnv = Map.insert name polyType env
      inferRec bodyEnv body

    Match obj patterns -> do
      objType <- inferRec env obj
      allBindings <- mapM (inferPattern objType . fst) patterns
      let extendEnv :: Map VarIdent MonoType -> Infer TypeEnv
          extendEnv bindings = Map.union env <$> mapM (generalize env) bindings
      bodyEnvs <- mapM (lift . extendEnv) allBindings
      bodyTypes <- zipWithMSafe inferRec bodyEnvs (map snd patterns)
      resultType <- lift fresh
      forM_ bodyTypes (unify resultType)
      return resultType

    App f args -> do
      fType <- inferRec env f
      -- the extra PolyType will unwrapped in inferCall with no side effects
      -- any type variables that need to be instantiated will already be in
      -- scope after the type is inferred
      inferCall env args $ PolyType [] fType

    If0 cond true false -> do
      -- infer call just instantiates the given polytype, so we don't need to worry
      -- about freshness because no other type in the environment can get involved
      let ident = TVarIdent 0
          t = TVar ident
          if0Type = PolyType [ident] (TLam [numType, t, t] t)
      inferCall env [cond, true, false] if0Type

    BinOp op a b -> inferCall env [a, b] (binOpType op)

  (InferState _ s2) <- get
  -- trace ("\n<<< " ++ pretty expr ++ " : " ++ pretty ret ++ prettyState s2) $ return ()
  return ret
  -- where
  --   prettyState s = "\n  env - " ++ prettyEnv ++ "\n  var - " ++ prettySub s
  --   prettyEnv = intercalate "\n        " $ map prettyEnvEntry $ Map.toList env
  --   prettyEnvEntry (VarIdent name, t) = name ++ ": " ++ pretty t
  --   prettySub = intercalate "\n        " . map prettySubEntry . Map.toList
  --   prettySubEntry (tv, t) = pretty (TVar tv) ++ ": " ++ pretty t


inferCall :: TypeEnv -> [Expr] -> PolyType -> ExceptT String Infer MonoType
inferCall env args fnPolyType = do
  fnType <- lift $ instantiate fnPolyType
  argTypes <- mapM (inferRec env) args
  resultType <- lift fresh
  unify fnType (TLam argTypes resultType)
  return resultType


-- | Infer the type of a case pattern and return types of all variable bindings
inferPattern :: MonoType -> MatchPattern -> ExceptT String Infer (Map VarIdent MonoType)
inferPattern objType (VarBinding ident) = do
  monoType <- lift fresh
  unify objType monoType
  return $ Map.singleton ident monoType
inferPattern objType (ConsPattern consName subPatterns) = do
  (argTypes, structType) <- lookupConstructorType consName
  unify objType structType
  Map.unions <$> zipWithMSafe inferPattern argTypes subPatterns
