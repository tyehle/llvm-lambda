{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Prelude hiding (fail)

import Control.Monad.Fail
import Data.List (isPrefixOf)
import qualified Text.Megaparsec as M (eof, errorBundlePretty, parse)
import Data.Bifunctor (first)

import Expr
import ParseLisp


parse :: String -> String -> Either String Expr
parse filename = first M.errorBundlePretty . M.parse parser filename
  where
    parser = wholeFile >>= translation


translation :: MonadFail m => Lisp -> m Expr
translation sym@(Symbol _) = Ref <$> parseIdenifier sym
translation (String s) = fail $ "Unexpected string " ++ show s
-- translation bad@(DotList _ _) = fail $ "Invalid syntax: " ++ show bad
translation (Number i) = return . Nat . fromIntegral $ i
translation (Float f) = fail $ "Unexpected float " ++ show f
translation (List []) = fail "Unexpected nil"
translation (List [Symbol "+", a, b]) =
  Plus <$> translation a <*> translation b
translation (List [Symbol "-", a, b]) =
  Minus <$> translation a <*> translation b
translation (List [Symbol "*", a, b]) =
  Mult <$> translation a <*> translation b
translation (List [Symbol "/", a, b]) =
  Divide <$> translation a <*> translation b
translation (List [Symbol "if0", c, t, f]) =
  If0 <$> translation c <*> translation t <*> translation f
translation (List [Symbol "let", List [name, value], body]) =
  Let <$> parseIdenifier name <*> translation value <*> translation body
translation (List [Symbol "lambda", List args, body]) =
  Lambda <$> mapM parseIdenifier args <*> translation body
translation (List (fn : args)) =
  App <$> translation fn <*> mapM translation args


keywords :: [String]
keywords = ["+", "-", "*", "/", "if0", "let", "lambda"]


parseIdenifier :: MonadFail m => Lisp -> m String
parseIdenifier (Symbol name)
  | name `elem` keywords || isPrefixOf "__" name = fail $ "Invalid identifier: " ++ name
  | otherwise = return name
parseIdenifier bad = fail $ "Invalid identifier: " ++ show bad