module Hash (runScript, runInteractive) where

import Control.Applicative
import qualified Data.Map as M

import Parsing.HashParser
import Language.Commands (commands)
import Language.Exec (runHashProgram, ScriptState(..), prompt)

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

-- Helper method for running interactive mode.
runInteractive' :: Either FilePath ScriptState -> IO ()
runInteractive' state = do
  putStr $ prompt state
  hFlush stdout
  line <- getLine
  state' <- runString line state
  runInteractive' $ Right state'

-- Loads .hashrc file and creates initial state.
loadHashRc :: FilePath -> IO (Either FilePath ScriptState)
loadHashRc path = do
  hashRcPath <- fmap (\x -> combine x ".hashrc") getHomeDirectory
  fileExists <- doesFileExist hashRcPath
  if fileExists then do state <- runPath hashRcPath $ Left path
                        return $ Right state
                else return $ Right $ ScriptState "" path M.empty

-- Runs script at given path.
runPath :: FilePath -> Either FilePath ScriptState -> IO ScriptState
runPath path state = do
  script <- readFile path
  runString script state

-- Runs given script.
runString :: String -> Either FilePath ScriptState -> IO ScriptState
runString script state = do
  let exprs = parseScript script
  runHashProgram commands state exprs
