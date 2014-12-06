module Main (main) where

import System.Environment
import Hash

main :: IO ()
main = do
  args <- getArgs
  if (null args) then runInteractive
                 else runScript $ head args
