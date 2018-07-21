module LowLevel where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import qualified Expr as HL
import Scope
import Fresh

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
          | NewClos String Int
          | SetEnv Expr Int Expr Expr
          | GetEnv Expr Int
          deriving (Eq, Show)


instance Scope Expr where
  freeVars (Num _) = Set.empty
  freeVars (Plus a b) = freeVars a `Set.union` freeVars b
  freeVars (Minus a b) = freeVars a `Set.union` freeVars b
  freeVars (Let name binding body) = freeVars binding `Set.union` Set.delete name (freeVars binding)
  freeVars (Ref name) = Set.singleton name
  freeVars (App name args) = Set.unions $ map freeVars args
  freeVars (AppClos fn args) = Set.unions . map freeVars $ fn : args
  freeVars (NewClos fnName size) = Set.empty
  freeVars (SetEnv clos index binding body) = Set.unions $ map freeVars [binding, clos, body]
  freeVars (GetEnv clos index) = freeVars clos


runConvert :: HL.Expr -> Set String -> Prog
runConvert body globals = Prog (reverse newDefs) newBody
  where
    (newBody, newDefs) = runWriter . flip runReaderT globals . flip evalFreshT Map.empty . convert $ body


convert :: HL.Expr -> FreshT (ReaderT (Set String) (Writer [Def])) Expr
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
  let freeMap = Map.fromList $ zip (Set.toList free) [0..]
  name <- freshFunc
  body' <- convert body
  tell [Def name ("_env" : args) (subst freeMap (Ref "_env") body')]
  closName <- freshClos
  let setEnv name = SetEnv (Ref closName) (freeMap Map.! name) (Ref name)
  let envBindings = foldr setEnv (Ref closName) (Map.keys freeMap)
  return $ Let closName (NewClos name (Map.size freeMap)) envBindings

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


freshFunc :: FreshT (ReaderT (Set String) (Writer [Def])) String
freshFunc = do
  i <- next "_f"
  return $ "_f" ++ show i

freshClos :: FreshT (ReaderT (Set String) (Writer [Def])) String
freshClos = do
  i <- next "_c"
  return $ "_c" ++ show i


subst :: Map String Int -> Expr -> Expr -> Expr
subst vars env n@Num{} = n
subst vars env (Plus a b) = Plus (subst vars env a) (subst vars env b)
subst vars env (Let name binding body) = Let name binding' body'
  where
    binding' = subst vars env binding
    body' = subst (Map.delete name vars) env body
subst vars env (Ref name) = case Map.lookup name vars of
  Just index -> GetEnv env index
  Nothing    -> Ref name
subst vars env (AppClos func args) = AppClos (subst vars env func) (map (subst vars env) args)
subst vars env (App name args) = App name $ map (subst vars env) args
subst vars env nc@NewClos{} = nc
subst vars env (SetEnv clos index binding body) = SetEnv clos' index binding' body'
  where
    clos' = subst vars env clos
    binding' = subst vars env binding
    body' = subst vars env body
subst vars env (GetEnv clos index) = GetEnv (subst vars env clos) index
