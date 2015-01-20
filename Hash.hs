module Hash (runScript, runInteractive) where

import Parsing.HashParser
import Language.Commands (commands)
import Language.Exec (runHashProgram, ScriptState)

import System.Directory
import System.IO

-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript path = do
  script <- readFile path
  let exprs = parseScript script
  putStrLn $ unlines [show expr | expr <- exprs]
  _ <- runHashProgram commands (Left path) exprs
  return ()

-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = do
  dir <- getCurrentDirectory
  runInteractive' $ Left dir

runInteractive' :: Either FilePath ScriptState -> IO ()
runInteractive' state = do
  putStr "$ "
  hFlush stdout
  line <- getLine
  let exprs = parseScript line
  state' <- runHashProgram commands state exprs
  runInteractive' $ Right state'
