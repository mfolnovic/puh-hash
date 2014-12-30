module Hash (runScript, runInteractive) where

import Parsing.HashParser

-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript path = do
  script <- readFile path
  let commands = parseScript(script)
  putStrLn $ show commands
  return ()

-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = putStrLn "interactive"
