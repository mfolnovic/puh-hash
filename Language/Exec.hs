module Language.Exec (Command, ScriptState(..), runHashProgram) where

import Data.Char (isAlphaNum)
import Data.List (elemIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)

import Language.Expressions

import System.IO

-- A model of a command which is waiting for arguments and a state to run
type Command = [String] -> ScriptState -> IO ScriptState

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
  nextState <- runTopLevel ct initial x
  runHashProgram ct (Right nextState) xs
  where initial = ScriptState "" path M.empty
runHashProgram ct (Right state) (x:xs) = do
  nextState <- runTopLevel ct state x
  runHashProgram ct (Right nextState) xs

-- Calculates the result of a top-level command execution
runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel ct state (TLCmd cmd) = runAnyCommand ct state cmd
runTopLevel ct state (TLCnd cnd) = runIf ct state cnd
-- The rest of the module should consist of similar functions, calling each
-- other so that each expression is parsed by a lower-level function and the
-- result can be used in a higher-level function. The Command table and state
-- are passed around as necessary to evaluate commands, assignments and
-- variable substitution. A better way to pass around variables would be to
-- use the State monad or even the StateT monad transformer to wrap IO into it.

runAnyCommand :: CommandTable -> ScriptState -> Cmd -> IO ScriptState
runAnyCommand ct state cmd@(Cmd _ _ _ _ _) = runCommand ct state cmd
runAnyCommand ct state assign@(Assign _ _) = runAssign state assign

runCommand :: CommandTable -> ScriptState -> Cmd -> IO ScriptState
runCommand ct state (Cmd name args inDir outDir append) = do
  let command = fromJust $ M.lookup name' ct
  newState <- command args' state
  handleOutput vt newState outDir append
  return newState
  where name' = value vt name
        args' = map (value vt) (args ++ (case inDir of
                                          Just inDir -> [inDir]
                                          Nothing    -> []))
        vt = vartable state

handleOutput :: VarTable -> ScriptState -> Maybe Expr -> Bool -> IO ()
handleOutput _ (ScriptState output _ _) Nothing _ = putStr output
handleOutput vt (ScriptState output _ _) (Just expr) append = do
  let fileName = value vt expr
  if (append) then appendFile fileName output else writeFile fileName output

runCommands :: CommandTable -> ScriptState -> [Cmd] -> IO ScriptState
runCommands ct state [] = return state
runCommands ct state (x:xs) = do
  state' <- runAnyCommand ct state x
  runCommands ct state' xs

runAssign :: ScriptState -> Cmd -> IO ScriptState
runAssign st@(ScriptState _ _ vt) (Assign (Str var) val) = return $ st { vartable = vt' }
  where vt' = M.insert var val' vt
        val' = value vt val

runIf :: CommandTable -> ScriptState -> Conditional -> IO ScriptState
runIf ct state (If pred cthen) =
  if (evaluatePred vt pred) then runCommands ct state cthen else return state
  where vt = vartable state
runIf ct state (IfElse pred cthen celse) =
  if (evaluatePred vt pred) then runCommands ct state cthen
                           else runCommands ct state celse
  where vt = vartable state

evaluateComp :: VarTable -> Comp -> Bool
evaluateComp vt (CEQ a b) = (value vt a) == (value vt b)
evaluateComp vt (CNE a b) = (value vt a) /= (value vt b)
evaluateComp vt (CGE a b) = (value vt a) > (value vt b)
evaluateComp vt (CGT a b) = (value vt a) >= (value vt b)
evaluateComp vt (CLE a b) = (value vt a) <= (value vt b)
evaluateComp vt (CLT a b) = (value vt a) < (value vt b)
evaluateComp vt (CLI a) = not $ null $ value vt a

evaluatePred :: VarTable -> Pred -> Bool
evaluatePred vt (Pred comp) = evaluateComp vt comp

interpolation :: VarTable -> String -> String
interpolation vt str = case next of
    Just (i, name) -> interpolation vt $ (take i str) ++ (val name) ++ (drop (1 + i + length name) str)
    Nothing -> str
  where next = fmap withVar $ elemIndex '$' str
        withVar i = (i, takeWhile (\x ->  isAlphaNum x || x == '_') $ drop (i + 1) str)
        val x = fromMaybe "" $ M.lookup x vt

value :: VarTable -> Expr -> String
value vt (Str x) = interpolation vt x
value vt (Var x) = fromMaybe "" $ M.lookup x vt
