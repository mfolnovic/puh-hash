module Hash (runScript, runInteractive) where

import Control.Applicative

import Parsing.HashParser
import Language.Commands (commands)
import Language.Exec (runHashProgram, ScriptState(..))

import System.Directory
import System.FilePath
import System.IO

-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript path = do
  dir <- getCurrentDirectory
  state <- loadHashRc path
  _ <- runPath dir state
  return ()

-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = do
  dir <- getCurrentDirectory
  state <- loadHashRc dir
  runInteractive' state

runInteractive' :: Either FilePath ScriptState -> IO ()
runInteractive' state = do
  putStr "$ "
  hFlush stdout
  line <- getLine
  let exprs = parseScript line
  state' <- runHashProgram commands state exprs
  runInteractive' $ Right state'

loadHashRc :: FilePath -> IO (Either FilePath ScriptState)
loadHashRc path = do
  hashRcPath <- fmap (\x -> combine x ".hashrc") getHomeDirectory
  fileExists <- doesFileExist hashRcPath
  if fileExists then do state <- runPath hashRcPath $ Left path
                        return $ Right state
                else return $ Left path

runPath :: FilePath -> Either FilePath ScriptState -> IO ScriptState
runPath path state = do
  script <- readFile path
  let exprs = parseScript script
  putStrLn $ unlines [show expr | expr <- exprs]
  runHashProgram commands state exprs
