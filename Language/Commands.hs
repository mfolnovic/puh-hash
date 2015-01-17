module Language.Commands (commands) where

import Control.Monad

import Data.List
import qualified Data.Map as M
import Language.Exec (Command, ScriptState(..))

import System.Directory
import System.FilePath

-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands :: M.Map String Command
commands = M.fromList [
             ("echo", echo),
             ("cat", cat),
             ("ls", ls),
             ("pwd", pwd),
             ("cd", cd)
           ]

echo :: Command
echo x state = return $ state { output = unlines [intercalate " " x] }

cat :: Command
cat xs state = do
  files <- sequence $ map readFile xs
  return state { output = unlines files }

ls :: Command
ls xs state@(ScriptState _ wd _) = do
  let paths = if null xs then [wd] else xs
  files <- sequence $ map getDirectoryContents paths
  let lists = map (unlines . map takeFileName) files
  return state { output = unlines lists }

pwd :: Command
pwd _ state = do
  return state { output = unlines [wd state] }

cd :: Command
cd xs state@(ScriptState _ wd _) = do
  homeDir <- getHomeDirectory
  path <- if null xs then getHomeDirectory else return $ combine wd $ head xs
  return state { output = "", wd = path }
