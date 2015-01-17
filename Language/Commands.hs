module Language.Commands (commands) where

import Data.List
import qualified Data.Map as M
import Language.Exec (Command, ScriptState)

-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands :: M.Map String Command
commands = M.fromList [
             ("echo", echo)
           ]

echo :: Command
echo x state = do
  putStrLn $ intercalate " " x
  return state
