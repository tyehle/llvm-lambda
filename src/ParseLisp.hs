module ParseLisp where

import Control.Monad (void)
import Data.Char (isSpace, isPrint)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String


data Lisp = Symbol String
          | String String
          | Number Integer
          | Float Double
          | List [Lisp]
          deriving (Eq, Ord, Show)


sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment
  where
    lineComment = L.skipLineComment ";"
    blockComment = void $ symbol "#;" *> expression


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: String -> Parser String
symbol = L.symbol sc


symbolLit :: Parser String
symbolLit = label "symbol" . lexeme $ some (satisfy goodChar)
  where
    goodChar c = not (isSpace c) && notElem c invalid && isPrint c
    invalid = "()[]{}\",'`;#|\\"


stringLit :: Parser String
stringLit = label "string" . lexeme $ char '"' *> manyTill L.charLiteral (char '"')


integerLit :: Parser Integer
integerLit = label "integer" . lexeme $ L.signed (pure ()) L.decimal


floatLit :: Parser Double
floatLit = label "float" . lexeme $ L.signed (pure ()) L.float


list :: Parser [Lisp]
list = lexeme enclosed
  where
    enclosed = between (symbol "(") (symbol ")") items
           <|> between (symbol "[") (symbol "]") items
           <|> between (symbol "{") (symbol "}") items
    items = many expression


expression :: Parser Lisp
expression = label "expression" $ Float  <$> try floatLit
                              <|> Number <$> try integerLit
                              <|> Symbol <$> try symbolLit
                              <|> String <$> try stringLit
                              <|> List   <$> list

wholeFile :: Parser Lisp
wholeFile = sc *> expression <* eof
