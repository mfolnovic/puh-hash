module Language.Exec (Command, ScriptState(..), runHashProgram, prompt) where

import Data.Char (isAlphaNum, isNumber)
import Data.List (elemIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.String.Utils (replace)
import GHC.IO.Handle (hDuplicate)
import Language.Expressions
import Network.URI (unEscapeString)
import System.Directory
import System.IO

-- A model of a command which is waiting for arguments and a state to run
type Command = [String] -> Handle -> ScriptState -> IO ScriptState

-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String String

-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = M.Map String Command

-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState { output :: String
                               , wd :: FilePath
                               , vartable :: VarTable
                               } deriving Show

-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command’s execution.
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr]
                    -> IO ScriptState
runHashProgram ct (Left path) [] = return $ ScriptState "" path M.empty
runHashProgram _ (Right state) [] = return state
runHashProgram ct (Left path) (x:xs) = do
  currentDir <- getCurrentDirectory
  let initial = ScriptState "" currentDir M.empty
  nextState <- runTopLevel ct initial x
  runHashProgram ct (Right nextState) xs
runHashProgram ct (Right state) (x:xs) = do
  nextState <- runTopLevel ct state x
  runHashProgram ct (Right nextState) xs

-- Calculates the result of a top-level command execution
runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel ct state (TLCmd cmd) = runAnyCommand ct state cmd
runTopLevel ct state (TLCnd cnd) = runIf ct state cnd
runTopLevel ct state (TLLoop loop) = runLoop ct state loop

-- Runs given command or assignment.
runAnyCommand :: CommandTable -> ScriptState -> Cmd -> IO ScriptState
runAnyCommand ct state cmd@(Cmd _ _ _ _ _) = runCommand ct state cmd
runAnyCommand ct state assign@(Assign _ _) = runAssign state assign

-- Runs given command.
runCommand :: CommandTable -> ScriptState -> Cmd -> IO ScriptState
runCommand ct state (Cmd name args inDir outDir append) = do
  newState <- handleCommand ct state name' $ \cmd -> do
    handleInput state inDir $ \h -> cmd args' h state
  handleOutput newState outDir append
  return newState
  where name' = value state name
        args' = map (value state) args
        vt = vartable state

-- Handles if given command is not available.
handleCommand :: CommandTable -> ScriptState -> String
                   -> (Command -> IO ScriptState) -> IO ScriptState
handleCommand ct state name f =
  case M.lookup name ct of
    Just cmd -> f cmd
    Nothing  -> return state { output = "Unknown command: " ++ name }

-- Handles input (redirecting input from file or stdin)
handleInput :: ScriptState -> Maybe Expr -> (Handle -> IO ScriptState)
                 -> IO ScriptState
handleInput state (Just inDir) f = withFile (value state inDir) ReadMode f
handleInput _ Nothing f = f stdin

-- Handle output (redirect output to file or stdout)
handleOutput :: ScriptState -> Maybe Expr -> Bool -> IO ()
handleOutput (ScriptState output _ _) Nothing _ = putStr output'
  where output' = if (not (null output) && last output /= '\n') then output ++ "\n"
                                                                else output
handleOutput state@(ScriptState output _ _) (Just expr) append = do
  let fileName = value state expr
  if (append) then appendFile fileName output else writeFile fileName output

-- Runs given list of commands.
runCommands :: CommandTable -> ScriptState -> [Cmd] -> IO ScriptState
runCommands ct state [] = return state
runCommands ct state (x:xs) = do
  state' <- runAnyCommand ct state x
  runCommands ct state' xs

-- Runs assignment.
runAssign :: ScriptState -> Cmd -> IO ScriptState
runAssign st@(ScriptState _ _ vt) (Assign (Str var) val) = return $ st'
  where st' = st { vartable = vt' }
        vt' = M.insert var val' vt
        val' = value st val

-- Runs if.
runIf :: CommandTable -> ScriptState -> Conditional -> IO ScriptState
runIf ct state (If pred cthen) =
  if (evaluatePred state pred) then runCommands ct state cthen else return state
runIf ct state (IfElse pred cthen celse) =
  if (evaluatePred state pred) then runCommands ct state cthen
                               else runCommands ct state celse

-- Runs loop.
runLoop :: CommandTable -> ScriptState -> Loop -> IO ScriptState
runLoop ct state loop@(While pred commands) = do
  if (evaluatePred state pred) then do state' <- runCommands ct state commands
                                       runLoop ct state' loop
                               else return state

-- Evaluates comparison.
evaluateComp :: ScriptState -> Comp -> Bool
evaluateComp st (CEQ a b) = (numericValue st a) == (numericValue st b)
evaluateComp st (CNE a b) = (numericValue st a) /= (numericValue st b)
evaluateComp st (CGE a b) = (numericValue st a) > (numericValue st b)
evaluateComp st (CGT a b) = (numericValue st a) >= (numericValue st b)
evaluateComp st (CLE a b) = (numericValue st a) <= (numericValue st b)
evaluateComp st (CLT a b) = (numericValue st a) < (numericValue st b)
evaluateComp st (CLI a) = not $ null $ value st a

-- Evaluates predicate.
evaluatePred :: ScriptState -> Pred -> Bool
evaluatePred st (Pred comp) = evaluateComp st comp
evaluatePred st (Not pred) = not $ evaluatePred st pred
evaluatePred st (And a b) = (evaluatePred st a) && (evaluatePred st b)
evaluatePred st (Or a b) = (evaluatePred st a) && (evaluatePred st b)
evaluatePred st (Parens pred) = evaluatePred st pred

-- Interpolates variables in string.
interpolation :: ScriptState -> String -> String
interpolation state str = case next of
    Just (i, name) -> interpolation state $ (take i str) ++ (val name)
                      ++ (drop (1 + i + length name) str)
    Nothing -> str
  where next = fmap withVar $ elemIndex '$' str
        withVar i = (i, takeWhile inVar $ drop (i + 1) str)
        inVar x = isAlphaNum x || x == '_'
        val x = value state (Var x)

-- Default values
defaultValues = M.fromList [("PS1", "\x1b[0;31m$wd \x1b[0m# ")]

-- Returns numeric value of a given variable or interpolated string.
numericValue :: ScriptState -> Expr -> Int
numericValue state expr = if (all isNumber val) then read val else 0
  where val = value state expr

-- Returns value of a given variable or interpolates given string.
value :: ScriptState -> Expr -> String
value (ScriptState _ wd _) (Var "wd") = wd
value state (Str x) = unescape $ interpolation state x
value state (Var x) = unescape $ fromMaybe defVal $ M.lookup x (vartable state)
  where defVal = fromMaybe "" $ M.lookup x defaultValues

-- Unescapes string
unescape :: String -> String
unescape str = unEscapeString str'
  where str' = read $ "\"" ++ str ++ "\"" :: String

-- Prompt
prompt :: Either FilePath ScriptState -> String
prompt (Left _) = ""
prompt (Right state) = value state (Str "$PS1")
