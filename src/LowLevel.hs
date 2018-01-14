module LowLevel where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import qualified Expr as HL
import Scope

-- main :: IO ()
-- main = do
--   print $ Prog [] example
--   putStrLn "-- becomes --"
--   print . convertProg $ Prog [] example
--   where
--     -- f = lambda x: lambda y: x + y
--     -- plus1 = f(1)
--     -- plus2 = f(2)
--     -- plus1(3) + plus2(3)
--     example =
--       Let "f" (Lambda ["x"] (Lambda ["y"] (Plus (Ref "x") (Ref "y")))) $
--         Let "plus1" (Call (Ref "f") [Num 1]) $
--           Let "plus2" (Call (Ref "f") [Num 2]) $
--             Plus (Call (Ref "plus1") [Num 3]) (Call (Ref "plus2") [Num 3])

data Prog = Prog [Def] Expr deriving (Eq, Show)

data Def = Def String [String] Expr deriving (Eq, Show)

data Expr = Num Int
          | Plus Expr Expr
          | Minus Expr Expr
          | Let String Expr Expr
          | Ref String
          | App String [Expr]
          | AppClos Expr [Expr]
          | NewClos String
          | SetEnv String Expr Expr Expr
          | GetEnv String Expr
          deriving (Eq, Show)


instance Scope Expr where
  freeVars (Num _) = Set.empty
  freeVars (Plus a b) = freeVars a `Set.union` freeVars b
  freeVars (Minus a b) = freeVars a `Set.union` freeVars b
  freeVars (Let name binding body) = freeVars binding `Set.union` Set.delete name (freeVars binding)
  freeVars (Ref name) = Set.singleton name
  freeVars (App name args) = Set.unions $ map freeVars args
  freeVars (AppClos fn args) = Set.unions . map freeVars $ fn : args
  freeVars (NewClos fnName) = Set.empty
  freeVars (SetEnv name binding clos body) = Set.unions $ map freeVars [binding, clos, body]
  freeVars (GetEnv name clos) = freeVars clos


runConvert :: HL.Expr -> Set String -> Prog
runConvert body globals = Prog (reverse newDefs) newBody
  where
    (newBody, newDefs) = runWriter . flip runReaderT globals . flip evalStateT (0, 0) . convert $ body


convert :: HL.Expr -> StateT (Int, Int) (ReaderT (Set String) (Writer [Def])) Expr
convert (HL.Nat n) = return $ Num n
convert (HL.Plus a b) = do
  a' <- convert a
  b' <- convert b
  return $ Plus a' b'
convert (HL.Minus a b) = do
  a' <- convert a
  b' <- convert b
  return $ Minus a' b'
convert (HL.Let name binding body) = do
  bind <- convert binding
  body' <- convert body
  return $ Let name bind body'
convert (HL.Ref r) = return $ Ref r

convert (HL.Lambda args body) = do
  let free = freeVars body `Set.difference` Set.fromList args
  name <- freshFunc
  body' <- convert body
  tell [Def name ("_env" : args) (subst free (Ref "_env") body')]
  closName <- freshClos
  let setEnv name = SetEnv name (Ref closName) (Ref name)
  let envBindings = foldr setEnv (Ref closName) (Set.toList free)
  return $ Let closName (NewClos name) envBindings

convert (HL.App (HL.Ref name) args) = do
  args' <- mapM convert args
  isGlobal <- Set.member name <$> ask
  if isGlobal
    then return $ App name args'
    else return $ callClosure (Ref name) args'
convert (HL.App fn args) = do
  fn' <- convert fn
  args' <- mapM convert args
  return $ callClosure fn' args'


callClosure :: Expr -> [Expr] -> Expr
callClosure closure args = AppClos closure $ closure : args


freshFunc :: StateT (Int, Int) (ReaderT (Set String) (Writer [Def])) String
freshFunc = do
  (f, c) <- get
  put (f+1, c)
  return $ "_f" ++ show f

freshClos :: StateT (Int, Int) (ReaderT (Set String) (Writer [Def])) String
freshClos = do
  (f, c) <- get
  put (f, c+1)
  return $ "_c" ++ show c


subst :: Set String -> Expr -> Expr -> Expr
subst vars env n@Num{} = n
subst vars env (Plus a b) = Plus (subst vars env a) (subst vars env b)
subst vars env (Let name binding body) = Let name binding' body'
  where
    binding' = subst vars env binding
    body' = subst (Set.delete name vars) env body
subst vars env (Ref name)
  | Set.member name vars = GetEnv name env
  | otherwise            = Ref name
-- subst vars env (HL.Lambda args body) = Lambda args $ subst (vars `Set.difference` Set.fromList args) env body
subst vars env (AppClos func args) = AppClos (subst vars env func) (map (subst vars env) args)
subst vars env (App name args) = App name $ map (subst vars env) args
subst vars env nc@NewClos{} = nc
subst vars env (SetEnv name clos binding body) = SetEnv name clos' binding' body'
  where
    clos' = subst vars env clos
    binding' = subst vars env binding
    body' = subst vars env body
subst vars env (GetEnv name clos) = GetEnv name $ subst vars env clos
