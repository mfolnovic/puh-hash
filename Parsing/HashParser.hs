module Parsing.HashParser (parseScript, scriptParser, ifelseexpr) where

import Control.Applicative (Applicative, many, (<$), (<*>), (<$>), (<|>), (*>), (<*))
import Control.Monad (void)
import Data.Either

import Language.Expressions

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (alphaNum, anyChar, letter, char, digit, oneOf, noneOf, string)
import Text.Parsec.Combinator (choice, manyTill, many1, eof, optionMaybe)
import Text.ParserCombinators.Parsec (try, parse)

parseScript :: String -> [TLExpr]
parseScript str =
  case result of Left err -> error $ show err
                 Right result -> result
  where result = parse scriptParser "(unknown)" str

scriptParser :: Parser [TLExpr]
scriptParser = many $ empty *> (try tlcnd <|> try tlloop <|> try tlcmd)

tlcmd :: Parser TLExpr
tlcmd = TLCmd <$> assignOrCommand

tlcnd :: Parser TLExpr
tlcnd = TLCnd <$> (try ifelseexpr <|> ifexpr) <* empty

tlloop :: Parser TLExpr
tlloop = TLLoop <$> while

commands :: Parser [Cmd]
commands = many $ empty *> (try assign <|> command)

lexeme :: Parser a -> Parser a
lexeme p = p <* space

empty :: Parser ()
empty = void $ many $ (try comment) <|> whitespace

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a] 
a <:> b = (:) <$> a <*> b

whitespace :: Parser ()
whitespace = void $ many1 $ oneOf " \n\t"

comment :: Parser ()
comment = do
  _ <- char '#'
  _ <- void $ many $ noneOf "\n"
  return ()

space :: Parser ()
space = void $ many $ char ' '

identifier :: Parser String
identifier = lexeme $ firstChar <:> many nonFirstChar
  where firstChar = letter <|> char '_'
        nonFirstChar = digit <|> firstChar

stringParser :: Parser Expr
--stringParser = try nonQuotedString <|> quotedString
stringParser = try quotedString <|> nonQuotedString

nonQuotedString :: Parser Expr
nonQuotedString = Str <$> parsed
  where parsed = lexeme $ many1 $ noneOf " <>#\"\n\t"

quotedString :: Parser Expr
quotedString = do
  _ <- char '"'
  result <- many $ noneOf "\""
  _ <- char '"'
  return $ Str result

varParser :: Parser Expr
varParser = (char '$') *> (Var <$> identifier)

expr :: Parser Expr
expr = try varParser <|> stringParser

comp :: (Expr -> Expr -> Comp) -> String -> Parser Comp
comp t op = do
  e1 <- lexeme expr
  _ <- lexeme $ string op
  e2 <- lexeme expr
  return $ t e1 e2

comparison :: Parser Comp
comparison = (try $ comp CEQ "==") <|>
             (try $ comp CNE "/=") <|>
             (try $ comp CGE ">=") <|>
             (try $ comp CGT ">")  <|>
             (try $ comp CLE "<=") <|>
             (try $ comp CLT "<")  <|>
             (CLI <$> expr)

notPredicate :: Parser Pred
notPredicate = (lexeme $ char '!') *> (Not <$> predicate)

singlePredicate :: Parser Pred
singlePredicate = Pred <$> comparison

parensPredicate :: Parser Pred
parensPredicate = (lexeme $ char '(') *> (predicate) <* (lexeme $ char ')')

orAndPredicate :: (Pred -> Pred -> Pred) -> String -> Parser Pred
orAndPredicate t s = do
  p1 <- lexeme simplePredicate
  _ <- lexeme $ string s
  p2 <- lexeme simplePredicate
  return $ t p1 p2

simplePredicate :: Parser Pred
simplePredicate = try notPredicate <|> try parensPredicate <|> singlePredicate

predicate :: Parser Pred
predicate = (try $ orAndPredicate Or "-o") <|> (try $ orAndPredicate And "-a") <|> simplePredicate

assign :: Parser Cmd
assign = do
  name <- Str <$> identifier
  _ <- lexeme $ char '='
  value <- expr
  _ <- empty
  return $ Assign name value

command :: Parser Cmd
command = do
  name <- expr
  args <- many expr
  _ <- optionMaybe whitespace
  inDir <- optionMaybe inDirP
  outDirAppend <- optionMaybe outDirP
  let (outDir, append) = case outDirAppend of
                           Just (outDir, append) -> (Just outDir, append)
                           Nothing              -> (Nothing, False)
  _ <- empty
  return $ Cmd name args inDir outDir append

outDirP :: Parser (Expr, Bool)
outDirP = do
  t <- lexeme $ (try $ string ">>") <|> string ">"
  e <- lexeme expr
  let append = t == ">>"
  return (e, append)

inDirP :: Parser Expr
inDirP = do
  _ <- lexeme $ string "<"
  e <- lexeme expr
  return e

assignOrCommand :: Parser Cmd
assignOrCommand = try assign <|> command

ifexpr :: Parser Conditional
ifexpr = do
  _ <- string "if" <* whitespace
  pred <- predicate
  _ <- string "then" <* empty
  cthen <- manyTill assignOrCommand (try $ string "fi")
  return $ If pred cthen

ifelseexpr :: Parser Conditional
ifelseexpr = do
  _ <- string "if" <* whitespace
  pred <- predicate
  _ <- string "then" <* empty
  cthen <- manyTill assignOrCommand (try $ string "else")
  _ <- empty
  celse <- manyTill assignOrCommand (try $ string "fi")
  return $ IfElse pred cthen celse

while :: Parser Loop
while = do
  _ <- string "while" <* whitespace
  pred <- predicate
  _ <- string "do" <* empty
  commands <- manyTill assignOrCommand (try $ string "done")
  return $ While pred commands
