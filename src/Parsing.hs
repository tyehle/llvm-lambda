{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Prelude hiding (fail)

import Control.Monad.Fail
import Data.Bifunctor (first)
import Data.List (isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.Megaparsec as M (errorBundlePretty, parse)

import Expr
import ParseLisp


parse :: String -> String -> Either String Expr
parse filename = first M.errorBundlePretty . M.parse parser filename
  where
    parser = wholeFile >>= translate


translate :: MonadFail m => Lisp -> m Expr
translate expr = case expr of
  -- atoms
  Symbol _ -> Ref <$> parseIdentifier expr
  String s -> fail $ "Unexpected string" ++ show s
  Number n -> return $ Nat $ fromIntegral n
  Float f -> fail $ "Unexpected float" ++ show f
  List [] -> fail "Unexpected nil"
  -- keywords
  List (Symbol name : exprs) | Set.member name keywords -> parseKeyword name exprs
  -- assume application if the list does not start with a keyword
  List (fn : args) -> App <$> translate fn <*> mapM translate args


keywords :: Set String
keywords = Set.fromList ["lambda", "let", "letrec", "if0", "+", "-", "*", "/"]


parseKeyword :: MonadFail m => String -> [Lisp] -> m Expr
parseKeyword keyword exprs = case keyword of
  "lambda" -> case exprs of
    [List args, body] -> Lambda <$> mapM parseIdentifier args <*> translate body
    _ -> syntaxError

  "let" -> case exprs of
    [List [name, value], body] -> Let <$> parseIdentifier name <*> translate value <*> translate body
    _ -> syntaxError

  -- "letrec" -> case exprs of
  --   [List [name, value], body] -> Letrec <$> parseIdentifier name <*> translate value <*> translate body
  --   _ -> syntaxError

  "if0" -> case exprs of
    [c, t, f] -> If0 <$> translate c <*> translate t <*> translate f
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
      [a, b] -> BinOp op <$> translate a <*> translate b
      _ -> syntaxError


parseIdentifier :: MonadFail m => Lisp -> m String
parseIdentifier expr = case expr of
  (Symbol name) | invalid name -> fail $ "Invalid identifier: " ++ name
  (Symbol name) -> return name
  bad -> fail $ "Invalid identifier: " ++ show bad
  where
    invalid name = Set.member name keywords || isPrefixOf "__" name
