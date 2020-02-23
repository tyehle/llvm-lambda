{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Prelude hiding (fail)

import Control.Monad.Fail
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 (isSpace_w8)
import Data.Attoparsec.Number (Number(..))
import qualified Data.AttoLisp as L
import Data.ByteString.UTF8
import Data.Text as T

import Expr


parse :: String -> Either String Expr
parse input = parseOnly parser (fromString input)
  where
    parser = L.lisp <* A.takeWhile isSpace_w8 <* endOfInput >>= translation


translation :: MonadFail m => L.Lisp -> m Expr
translation sym@(L.Symbol _) = Ref <$> parseIdenifier sym
translation (L.String s) = fail $ "Unexpected string " ++ show s
translation bad@(L.DotList _ _) = fail $ "Invalid syntax: " ++ show bad
translation (L.Number (I integer)) = return . Nat . fromIntegral $ integer
translation (L.Number (D f)) = fail $ "Unexpected float " ++ show f
translation (L.List []) = fail "Unexpected nil"
translation (L.List [L.Symbol "+", a, b]) =
  Plus <$> translation a <*> translation b
translation (L.List [L.Symbol "-", a, b]) =
  Minus <$> translation a <*> translation b
translation (L.List [L.Symbol "*", a, b]) =
  Mult <$> translation a <*> translation b
translation (L.List [L.Symbol "/", a, b]) =
  Divide <$> translation a <*> translation b
translation (L.List [L.Symbol "if0", c, t, f]) =
  If0 <$> translation c <*> translation t <*> translation f
translation (L.List [L.Symbol "let", L.List [name, value], body]) =
  Let <$> parseIdenifier name <*> translation value <*> translation body
translation (L.List [L.Symbol "lambda", L.List args, body]) =
  Lambda <$> mapM parseIdenifier args <*> translation body
translation (L.List (fn : args)) =
  App <$> translation fn <*> mapM translation args


keywords :: [Text]
keywords = ["+", "-", "*", "/", "if0", "let", "lambda"]


parseIdenifier :: MonadFail m => L.Lisp -> m String
parseIdenifier (L.Symbol name)
  | name `elem` keywords = fail $ "Invalid identifier: " ++ T.unpack name
  | otherwise = return $ T.unpack name
parseIdenifier bad = fail $ "Invalid identifier: " ++ show bad