module LowLevel where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad.Reader

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

data Def = ClosureDef String String [String] Expr deriving (Eq, Show)

data Expr = Num Int
          | Plus Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Divide Expr Expr
          | Let String Expr Expr
          | Ref String
          | App String [Expr]
          | AppClos Expr [Expr]
          | NewClos String [Expr]
          | GetEnv Expr Integer
          deriving (Eq, Show)


instance Scope Expr where
  freeVars (Num _) = Set.empty
  freeVars (Plus a b) = freeVars a `Set.union` freeVars b
  freeVars (Minus a b) = freeVars a `Set.union` freeVars b
  freeVars (Mult a b) = freeVars a `Set.union` freeVars b
  freeVars (Divide a b) = freeVars a `Set.union` freeVars b
  freeVars (Let name binding body) = freeVars binding `Set.union` Set.delete name (freeVars body)
  freeVars (Ref name) = Set.singleton name
  freeVars (App name args) = Set.unions $ Set.singleton name : map freeVars args
  freeVars (AppClos fn args) = Set.unions . map freeVars $ fn : args
  freeVars (NewClos fnName bindings) = Set.unions $ Set.singleton fnName : map freeVars bindings
  freeVars (GetEnv clos _) = freeVars clos


runConvert :: HL.Expr -> Set String -> Prog
runConvert body globals = Prog (reverse newDefs) newBody
  where
    (newBody, newDefs) = runWriter . flip runReaderT globals . flip evalFreshT Map.empty . convert $ body


convertNumBinOp :: HL.Expr -> HL.Expr -> (Expr -> Expr -> Expr) -> FreshT (ReaderT (Set String) (Writer [Def])) Expr
convertNumBinOp a b op = do
  a' <- convert a
  b' <- convert b
  return $ op a' b'


convert :: HL.Expr -> FreshT (ReaderT (Set String) (Writer [Def])) Expr
convert (HL.Nat n) = return $ Num n
convert (HL.Plus a b) = convertNumBinOp a b Plus
convert (HL.Minus a b) = convertNumBinOp a b Minus
convert (HL.Mult a b) = convertNumBinOp a b Mult
convert (HL.Divide a b) = convertNumBinOp a b Divide
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
  tell [ClosureDef name "_env" args (subst freeMap (Ref "_env") body')]
  return . NewClos name . map Ref . Map.keys $ freeMap

convert (HL.App (HL.Ref name) args) = do
  args' <- mapM convert args
  isGlobal <- Set.member name <$> ask
  if isGlobal
    then return $ App name args'
    else return $ AppClos (Ref name) args'
convert (HL.App fn args) = do
  fn' <- convert fn
  args' <- mapM convert args
  return $ AppClos fn' args'


freshFunc :: FreshT (ReaderT (Set String) (Writer [Def])) String
freshFunc = do
  i <- next "_f"
  return $ "_f" ++ show i


subst :: Map String Integer -> Expr -> Expr -> Expr
subst _ _ n@Num{} = n
subst vars env (Plus a b) = Plus (subst vars env a) (subst vars env b)
subst vars env (Minus a b) = Minus (subst vars env a) (subst vars env b)
subst vars env (Mult a b) = Mult (subst vars env a) (subst vars env b)
subst vars env (Divide a b) = Divide (subst vars env a) (subst vars env b)
subst vars env (Let name binding body) = Let name binding' body'
  where
    binding' = subst vars env binding
    body' = subst (Map.delete name vars) env body
subst vars env (Ref name) = case Map.lookup name vars of
  Just index -> GetEnv env index
  Nothing    -> Ref name
subst vars env (AppClos func args) = AppClos (subst vars env func) (map (subst vars env) args)
subst vars env (App name args) = App name $ map (subst vars env) args
subst vars env (NewClos name bindings) = NewClos name $ map (subst vars env) bindings
subst vars env (GetEnv clos index) = GetEnv (subst vars env clos) index
