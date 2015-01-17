module Parsing.HashParser (parseScript) where

import Control.Applicative (Applicative, many, (<$), (<*>), (<$>), (<|>), (*>), (<*))
import Control.Monad (void)
import Data.Either

import Language.Expressions

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (alphaNum, anyChar, letter, char, digit, oneOf, noneOf)
import Text.Parsec.Combinator (choice, manyTill, many1, eof)
import Text.ParserCombinators.Parsec (try, parse)

parseScript :: String -> [TLExpr]
parseScript str =
  case result of Left err -> error $ show err
                 Right result -> result
  where result = parse scriptParser "(unknown)" str

scriptParser :: Parser [TLExpr]
scriptParser = many $ TLCmd <$> command

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

lexeme' :: Parser a -> Parser a
lexeme' p = p <* space

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a] 
a <:> b = (:) <$> a <*> b

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

space :: Parser ()
space = void $ many $ char ' '

identifier :: Parser String
identifier = lexeme $ firstChar <:> many nonFirstChar
  where firstChar = letter <|> char '_'
        nonFirstChar = digit <|> firstChar

stringParser :: Parser Expr
stringParser = try nonQuotedString <|> quotedString

nonQuotedString :: Parser Expr
nonQuotedString = Str <$> parsed
  where parsed = lexeme' $ many1 $ noneOf " \"\n\t"

quotedString :: Parser Expr
quotedString = do
  char '"'
  result <- many $ noneOf "\""
  char '"'
  return $ Str result

varParser :: Parser Expr
varParser = (char '$') *> (Var <$> identifier)

expr :: Parser Expr
expr = try stringParser <|> varParser

command :: Parser Cmd
command = do
  name <- expr
  args <- many expr
  _ <- whitespace
  return $ Cmd name args Nothing Nothing False
