module Parsing.HashParser (parseScript, scriptParser, ifelseexpr) where

import Control.Applicative (Applicative, many, (<$), (<*>), (<$>), (<|>), (*>),
                            (<*))
import Control.Monad (void)
import Data.Either

import Language.Expressions

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (alphaNum, anyChar, letter, char, digit, oneOf, noneOf,
                         string)
import Text.Parsec.Combinator (choice, manyTill, many1, eof, optionMaybe)
import Text.ParserCombinators.Parsec (try, parse)

-- Handles errors while parsing whole script to expressions.
parseScript :: String -> [TLExpr]
parseScript str =
  case result of Left err -> error $ show err
                 Right result -> result
  where result = parse scriptParser "(unknown)" str

-- Parses whole script to expressions.
scriptParser :: Parser [TLExpr]
scriptParser = many $ empty *> (try tlcnd <|> try tlloop <|> try tlcmd)

-- Parses command or assign.
tlcmd :: Parser TLExpr
tlcmd = TLCmd <$> assignOrCommand

-- Parses if or ifelse.
tlcnd :: Parser TLExpr
tlcnd = TLCnd <$> (try ifelseexpr <|> ifexpr) <* empty

-- Parses loop.
tlloop :: Parser TLExpr
tlloop = TLLoop <$> while

-- Parses list of commands.
commands :: Parser [Cmd]
commands = many $ empty *> (try assign <|> command)

-- Removes all space after parsed.
lexeme :: Parser a -> Parser a
lexeme p = p <* space

-- Takes all whitespaces and comments.
empty :: Parser ()
empty = void $ many $ (try comment) <|> whitespace

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a] 
a <:> b = (:) <$> a <*> b

-- Parses whitespace.
whitespace :: Parser ()
whitespace = void $ many1 $ oneOf " \n\t"

-- Parses comments.
comment :: Parser ()
comment = do
  _ <- char '#'
  _ <- void $ many $ noneOf "\n"
  return ()

-- Parses space.
space :: Parser ()
space = void $ many $ char ' '

-- Parses identifiers (alphaNum or _).
identifier :: Parser String
identifier = lexeme $ firstChar <:> many nonFirstChar
  where firstChar = letter <|> char '_'
        nonFirstChar = digit <|> firstChar

-- Parses string (quoted or non-quoted).
stringParser :: Parser Expr
stringParser = try quotedString <|> nonQuotedString

-- Parses non-quoted string.
nonQuotedString :: Parser Expr
nonQuotedString = Str <$> parsed
  where parsed = lexeme $ many1 $ noneOf " <>#\"\n\t"

-- Parses quoted string.
quotedString :: Parser Expr
quotedString = do
  _ <- char '"'
  result <- many $ noneOf "\""
  _ <- char '"'
  return $ Str result

-- Parses variable.
varParser :: Parser Expr
varParser = (char '$') *> (Var <$> identifier)

-- Parser variable or string.
expr :: Parser Expr
expr = try varParser <|> stringParser

-- Parses complex comparison.
comp :: (Expr -> Expr -> Comp) -> String -> Parser Comp
comp t op = do
  e1 <- lexeme expr
  _ <- lexeme $ string op
  e2 <- lexeme expr
  return $ t e1 e2

-- Parses single comparison.
comparison :: Parser Comp
comparison = (try $ comp CEQ "==") <|>
             (try $ comp CNE "/=") <|>
             (try $ comp CGE ">=") <|>
             (try $ comp CGT ">")  <|>
             (try $ comp CLE "<=") <|>
             (try $ comp CLT "<")  <|>
             (CLI <$> expr)

-- Parses not predicate.
notPredicate :: Parser Pred
notPredicate = (lexeme $ char '!') *> (Not <$> predicate)

-- Parses single predicate.
singlePredicate :: Parser Pred
singlePredicate = Pred <$> comparison

-- Parses predicate around parenthesis.
parensPredicate :: Parser Pred
parensPredicate = (lexeme $ char '(') *> predicate <* whitespace <* (lexeme $ char ')')

-- Parses or or and predicate.
orAndPredicate :: (Pred -> Pred -> Pred) -> String -> Parser Pred
orAndPredicate t s = do
  p1 <- lexeme simplePredicate
  _ <- lexeme $ string s
  p2 <- lexeme simplePredicate
  return $ t p1 p2

-- Parses simple predicate (not, parenthesis or single).
simplePredicate :: Parser Pred
simplePredicate = try notPredicate <|> try parensPredicate <|> singlePredicate

-- Parses any predicate.
predicate :: Parser Pred
predicate = (try $ orAndPredicate Or "-o") <|> (try $ orAndPredicate And "-a")
            <|> simplePredicate

-- Parses assignment.
assign :: Parser Cmd
assign = do
  name <- Str <$> identifier
  _ <- lexeme $ char '='
  value <- expr
  _ <- empty
  return $ Assign name value

-- Parses command.
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

-- Parses out redirection.
outDirP :: Parser (Expr, Bool)
outDirP = do
  t <- lexeme $ (try $ string ">>") <|> string ">"
  e <- lexeme expr
  let append = t == ">>"
  return (e, append)

-- Parses in redirection.
inDirP :: Parser Expr
inDirP = do
  _ <- lexeme $ string "<"
  e <- lexeme expr
  return e

-- Parses assign or command.
assignOrCommand :: Parser Cmd
assignOrCommand = try assign <|> command

-- Parses if.
ifexpr :: Parser Conditional
ifexpr = do
  _ <- string "if" <* whitespace
  pred <- predicate
  _ <- string "then" <* empty
  cthen <- manyTill assignOrCommand (try $ string "fi")
  return $ If pred cthen

-- Parses if-else.
ifelseexpr :: Parser Conditional
ifelseexpr = do
  _ <- string "if" <* whitespace
  pred <- predicate
  _ <- string "then" <* empty
  cthen <- manyTill assignOrCommand (try $ string "else")
  _ <- empty
  celse <- manyTill assignOrCommand (try $ string "fi")
  return $ IfElse pred cthen celse

-- Parses while.
while :: Parser Loop
while = do
  _ <- string "while" <* whitespace
  pred <- predicate
  _ <- string "do" <* empty
  commands <- manyTill assignOrCommand (try $ string "done")
  _ <- empty
  return $ While pred commands
