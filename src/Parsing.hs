{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parsing where

import Prelude hiding (fail)

import Control.Monad.Fail
import Data.Bifunctor (first)
import Data.Foldable (foldlM)
import Data.List (isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.Megaparsec as M (errorBundlePretty, parse)

import HighLevel
import ParseLisp
import Pretty


parse :: String -> String -> Either String Prog
parse filename = first M.errorBundlePretty . M.parse parser filename
  where
    parser = wholeFile >>= translate


translate :: MonadFail m => [Lisp] -> m Prog
translate = foldlM translateSingle $ Prog [] []
  where
    translateSingle (Prog defs exprs) s = case s of
      List (Symbol name : rest) | Set.member name defKeywords -> do
        def <- parseDef name rest
        pure $ Prog (def : defs) exprs
      other -> Prog defs . (:exprs) <$> parseExpr other


parseExpr :: MonadFail f => Lisp -> f Expr
parseExpr expr = case expr of
  -- atoms
  Symbol _ -> Ref <$> parseIdentifier expr
  String s -> fail $ "Unexpected string: " ++ show s
  Number n -> return $ Nat $ fromIntegral n
  Float f -> fail $ "Unexpected float: " ++ show f
  List [] -> fail "Unexpected nil"
  -- keywords
  List (Symbol name : exprs) | Set.member name keywords -> parseKeyword name exprs
  List (Symbol bad : _) | Set.member bad defKeywords -> fail $ "Unexpected definition: " ++ pretty expr
  -- assume application if the list does not start with a keyword
  List (fn : args) -> App <$> parseExpr fn <*> mapM parseExpr args


defKeywords :: Set String
defKeywords = Set.fromList ["struct"]


parseDef :: MonadFail m => String -> [Lisp] -> m Def
parseDef keyword exprs = case keyword of
  "struct" -> case exprs of
    (typeRef : constructors) ->
      StructDef <$> parseTypeRef typeRef <*> parseConstructors constructors
    _ -> fail $ "Invalid struct: " ++ pretty (List (Symbol "struct" : exprs))
  _ -> fail $ "Unknown keyword: " ++ keyword
  where
    parseTypeRef ref = case ref of
      (Symbol name) -> pure $ TypeRef (TypeIdent name) []
      (List (Symbol name : vars)) -> TypeRef (TypeIdent name) <$> mapM parseTypeVar vars
      bad -> fail $ "Invalid type: " ++ pretty bad
      where
        parseTypeVar (Symbol name) = pure $ TypeIdent name
        parseTypeVar bad = fail $ "Invalid type variable: " ++ pretty bad

    parseConstructors = mapM parseOne
      where
        parseOne (Symbol name) = pure (ConsIdent name, [])
        parseOne (List (Symbol name : arguments)) = (ConsIdent name,) <$> mapM parseTypeRef arguments
        parseOne bad = fail $ "Invalid constructor: " ++ pretty bad


keywords :: Set String
keywords = Set.fromList ["lambda", "let", "letrec", "case", "if0", "+", "-", "*", "/"]


parseKeyword :: MonadFail m => String -> [Lisp] -> m Expr
parseKeyword keyword exprs = case keyword of
  "lambda" -> case exprs of
    [List args, body] -> Lambda <$> mapM parseIdentifier args <*> parseExpr body
    _ -> syntaxError

  "let" -> case exprs of
    [List [name, value], body] -> Let <$> parseIdentifier name <*> parseExpr value <*> parseExpr body
    _ -> syntaxError

  "letrec" -> case exprs of
    [List [name, value], body] -> Letrec <$> parseIdentifier name <*> parseExpr value <*> parseExpr body
    _ -> syntaxError

  "case" -> case exprs of
    (expr : clauses) -> Case <$> parseExpr expr <*> mapM parseCaseClause clauses

  "if0" -> case exprs of
    [c, t, f] -> If0 <$> parseExpr c <*> parseExpr t <*> parseExpr f
    _ -> syntaxError

  "+" -> parseBinOp Add
  "-" -> parseBinOp Sub
  "*" -> parseBinOp Mul
  "/" -> parseBinOp Div
  -- "%" -> parseBinOp Mod

  _ -> fail $ "Unknown keyword: " ++ keyword
  where
    syntaxError :: MonadFail m => m a
    syntaxError = fail $ "Invalid syntax in " ++ keyword ++ " expression"

    parseBinOp :: MonadFail m => BinOp -> m Expr
    parseBinOp op = case exprs of
      [a, b] -> BinOp op <$> parseExpr a <*> parseExpr b
      _ -> syntaxError


parseCaseClause :: MonadFail m => Lisp -> m (CasePattern, Expr)
parseCaseClause (List [pattern, expr]) = do
  pat <- parseCasePattern pattern
  body <- parseExpr expr
  pure (pat, body)
  where
    parseCasePattern (Symbol name) = pure . VarBinding . VarIdent $ name
    parseCasePattern (List (Symbol name : args)) = ConsPattern (ConsIdent name) <$> mapM parseCasePattern args
    parseCasePattern bad = fail $ "Invalid case pattern: " ++ pretty bad
parseCaseClause bad = fail $ "Invalid case: " ++ pretty bad


parseIdentifier :: MonadFail m => Lisp -> m VarIdent
parseIdentifier expr = case expr of
  (Symbol name) | invalid name -> fail $ "Invalid identifier: " ++ name
  (Symbol name) -> return $ VarIdent name
  bad -> fail $ "Invalid identifier: " ++ show bad
  where
    invalid name = Set.member name keywords || isPrefixOf "__" name
