module Main (main) where

import System.Environment (getArgs)
import Hash

main :: IO ()
main = do
  args <- getArgs
  case args of [] -> runInteractive
               [path] -> runScript path
               _ -> error "Hash [path]"
