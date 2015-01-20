module Main (main) where

import System.Environment (getArgs)
import System.IO
import Hash

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  args <- getArgs
  case args of [] -> runInteractive
               [path] -> runScript path
               _ -> error "Hash [path]"
