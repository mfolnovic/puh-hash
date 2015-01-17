module Hash (runScript, runInteractive) where

import Parsing.HashParser
import Language.Commands (commands)
import Language.Exec (runHashProgram)

-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript path = do
  script <- readFile path
  let exprs = parseScript(script)
  putStrLn $ unlines [show expr | expr <- exprs]
  _ <- runHashProgram commands (Left path) exprs
  return ()

-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = putStrLn "interactive"
