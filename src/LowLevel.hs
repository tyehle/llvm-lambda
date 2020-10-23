module LowLevel where

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

-- | A function definition that can be called
data Def
  -- | The function definition for a closure. This type of definition has a
  -- name, environment name, a list of argument names, and a body
  = ClosureDef String String [String] Expr deriving (Eq, Show)

data Expr = Num Int
          | Plus Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Divide Expr Expr
          | If0 Expr Expr Expr
          | Let String Expr Expr
          | Ref String
          | App String [Expr]
          | AppClos Expr [Expr]
          | NewClos String [Expr]
          | GetEnv String Integer
          deriving (Eq, Show)


instance Scope Expr where
  freeVars expr = case expr of
    Num _ -> Set.empty
    Plus a b -> freeVars a `Set.union` freeVars b
    Minus a b -> freeVars a `Set.union` freeVars b
    Mult a b -> freeVars a `Set.union` freeVars b
    Divide a b -> freeVars a `Set.union` freeVars b
    If0 c t f -> freeVars c `Set.union` freeVars t `Set.union` freeVars f
    Let name binding body -> freeVars binding `Set.union` Set.delete name (freeVars body)
    Ref name -> Set.singleton name
    App name args -> Set.unions $ Set.singleton name : map freeVars args
    AppClos fn args -> Set.unions . map freeVars $ fn : args
    NewClos fnName bindings -> Set.unions $ Set.singleton fnName : map freeVars bindings
    GetEnv _ _ -> Set.empty

  substitute trySub expr = case trySub expr of
    Just ex -> ex
    Nothing -> case expr of
      Num _ -> expr
      Plus a b -> Plus (recur a) (recur b)
      Minus a b -> Minus (recur a) (recur b)
      Mult a b -> Mult (recur a) (recur b)
      Divide a b -> Divide (recur a) (recur b)
      If0 c t f -> If0 (recur c) (recur t) (recur f)
      Let name binding body -> Let name (recur binding) (recur body)
      Ref _ -> expr
      AppClos func args -> AppClos (recur func) (map recur args)
      App name args -> App name (map recur args)
      NewClos name bindings -> NewClos name (map recur bindings)
      GetEnv envName index -> GetEnv envName index
    where
      recur = substitute trySub


runConvert :: HL.Expr -> Set String -> Prog
runConvert body globals = Prog (reverse newDefs) newBody
  where
    (newBody, newDefs) = runWriter . flip runReaderT globals . flip evalFreshT Map.empty . convert $ body


convertNumBinOp :: HL.Expr -> HL.Expr -> HL.BinOp -> FreshT (ReaderT (Set String) (Writer [Def])) Expr
convertNumBinOp a b op = do
  a' <- convert a
  b' <- convert b
  return $ constructor a' b'
  where
    constructor = case op of
      HL.Add -> Plus
      HL.Sub -> Minus
      HL.Mul -> Mult
      HL.Div -> Divide


convert :: HL.Expr -> FreshT (ReaderT (Set String) (Writer [Def])) Expr
convert (HL.Nat n) = return $ Num n

convert (HL.BinOp op a b) = convertNumBinOp a b op

convert (HL.If0 c t f) = do
  c' <- convert c
  t' <- convert t
  f' <- convert f
  return $ If0 c' t' f'

convert (HL.Let name binding body) = do
  bind <- convert binding
  body' <- convert body
  return $ Let name bind body'

convert (HL.Letrec name binding body) = do
  let sub expr
        | expr == (HL.Ref name) = Just $ HL.App (HL.Ref name) [HL.Ref name]
        | otherwise = Nothing
      binding' = HL.Lambda [name] $ substitute sub binding
      body' = substitute sub body
  convert (HL.Let name binding' body')

convert (HL.Ref r) = return $ Ref r

convert expr@(HL.Lambda args body) = do
  let free = Set.toList $ freeVars expr
      indices = Map.fromList $ zip free [0..]
      sub (Ref name) = GetEnv "_env" <$> Map.lookup name indices
      sub _ = Nothing
  name <- freshFunc
  body' <- convert body
  tell [ClosureDef name "_env" args (substitute sub body')]
  return $ NewClos name $ map Ref free

convert (HL.App (HL.Ref name) args) = do
  args' <- mapM convert args
  isGlobal <- asks $ Set.member name
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
