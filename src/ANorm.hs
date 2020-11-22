module ANorm where

import Fresh
import qualified LowLevel as LL

data Prog = Prog [Def] Expr deriving (Eq, Show)

data Def = ClosureDef String String [String] Expr deriving (Eq, Show)

data AExpr = Ref String
           | GetEnv String Integer
           deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq, Ord, Show)

data Expr
  = Num Int
  | BinOp BinOp AExpr AExpr
  | If0 AExpr Expr Expr
  | Let
      String
      Expr
      Expr
  | App String [AExpr]
  | AppClos AExpr [AExpr]
  | NewClos String [AExpr]
  | Atomic AExpr
  deriving (Eq, Show)


freshBinding :: (Monad m, MonadFresh m) => String -> LL.Expr -> m (AExpr, Expr -> Expr)
freshBinding _ (LL.Ref name) = pure (Ref name, id)
freshBinding _ (LL.GetEnv envName index) = pure (GetEnv envName index, id)
freshBinding prefix value = do
  name <- (prefix ++) . show <$> next prefix
  normalValue <- aNormalizeExpr value
  return (Ref name, Let name normalValue)


bindMany :: (Monad m, MonadFresh m) => String -> [LL.Expr] -> m ([AExpr], Expr -> Expr)
bindMany prefix values = do
  binders <- sequence $ zipWith indexedBinding [0..] values
  return (map fst binders, \body -> foldr snd body binders)
  where
    indexedBinding :: (Monad m, MonadFresh m) => Int -> LL.Expr -> m (AExpr, Expr -> Expr)
    indexedBinding i = freshBinding $ prefix ++ show i ++ "_"


aNormalizeBinOp :: (Monad m, MonadFresh m) => String -> LL.Expr -> LL.Expr -> BinOp -> m Expr
aNormalizeBinOp name a b op = do
  (aRef, aLet) <- freshBinding ("_" ++ name ++ "_a_") a
  (bRef, bLet) <- freshBinding ("_" ++ name ++ "_b_") b
  return $ aLet $ bLet $ BinOp op aRef bRef


aNormalizeExpr :: (Monad m, MonadFresh m) => LL.Expr -> m Expr
aNormalizeExpr (LL.Num n) =
  pure $ Num n

aNormalizeExpr (LL.Plus a b) = aNormalizeBinOp "add" a b Add
aNormalizeExpr (LL.Minus a b) = aNormalizeBinOp "sub" a b Sub
aNormalizeExpr (LL.Mult a b) = aNormalizeBinOp "mul" a b Mul
aNormalizeExpr (LL.Divide a b) = aNormalizeBinOp "div" a b Div

aNormalizeExpr (LL.If0 c t f) = do
  (cRef, cLet) <- freshBinding "_if_c_" c
  tValue <- aNormalizeExpr t
  fValue <- aNormalizeExpr f
  return $ cLet $ If0 cRef tValue fValue

aNormalizeExpr (LL.Let name value body) = Let name <$> aNormalizeExpr value <*> aNormalizeExpr body

aNormalizeExpr (LL.Ref name) = pure $ Atomic $ Ref name

aNormalizeExpr (LL.App name args) = do
  (refs, binding) <- bindMany "_arg" args
  return $ binding $ App name refs

aNormalizeExpr (LL.AppClos closure args) = do
  (cRef, cLet) <- freshBinding "_clos_" closure
  (refs, binding) <- bindMany "_arg" args
  return $ cLet $ binding $ AppClos cRef refs

aNormalizeExpr (LL.NewClos functionName envVars) = do
  (refs, binding) <- bindMany "_envVar" envVars
  return $ binding $ NewClos functionName refs

aNormalizeExpr (LL.GetEnv envName index) = pure $ Atomic $ GetEnv envName index


aNormalizeDef :: (Monad m, MonadFresh m) => LL.Def -> m Def
aNormalizeDef (LL.ClosureDef name envName argNames body) = ClosureDef name envName argNames <$> aNormalizeExpr body


aNormalizeProg :: (Monad m, MonadFresh m) => LL.Prog -> m Prog
aNormalizeProg (LL.Prog defs main) = do
  defs' <- mapM aNormalizeDef defs
  main' <- aNormalizeExpr main
  return $ Prog defs' main'