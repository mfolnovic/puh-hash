module Language.Commands (commands) where

import Control.Monad

import Data.List
import qualified Data.Map as M
import Language.Exec (Command, ScriptState(..))

-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands :: M.Map String Command
commands = M.fromList [
             ("echo", echo),
             ("cat", cat)
           ]

echo :: Command
echo x state = return $ state { output = unlines [intercalate " " x] }

cat :: Command
cat xs state = do
  files <- sequence $ map readFile xs
  return state { output = unlines files }
