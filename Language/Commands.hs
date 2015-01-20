module Language.Commands (commands) where

import Control.Exception
import Control.Monad

import Data.List
import qualified Data.Map as M
import Language.Exec (Command, ScriptState(..))

import System.Directory
import System.IO
import System.IO.Error
import System.FilePath

-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands :: M.Map String Command
commands = M.fromList [
             ("echo", echo),
             ("cat", cat),
             ("ls", ls),
             ("pwd", pwd),
             ("cd", cd),
             ("create", create),
             ("mv", mv),
             ("cp", cp),
             ("rm", rm),
             ("cpdir", cpdir),
             ("mkdir", mkdir),
             ("rmdir", rmdir)
           ]

echo :: Command
echo x state = return $ state { output = unlines [intercalate " " x] }

cat :: Command
cat xs state = do
  files <- sequence $ map readFile xs
  return state { output = unlines files }

ls :: Command
ls xs state@(ScriptState _ wd _) = do
  let paths = if null xs then [wd] else xs
  files <- sequence $ map getDirectoryContents paths
  let lists = map (unlines . map takeFileName) files
  return state { output = unlines lists }

pwd :: Command
pwd _ state = do
  return state { output = unlines [wd state] }

cd :: Command
cd xs state@(ScriptState _ wd _) = do
  homeDir <- getHomeDirectory
  path <- if null xs then getHomeDirectory else return $ combine wd $ head xs
  return state { output = "", wd = path }

create :: Command
create xs state@(ScriptState _ wd _) = do
  let paths = map (combine wd) xs
  handles <- sequence $ map (\x -> openFile x AppendMode) paths
  return state { output = "" }

mv :: Command
mv xs state@(ScriptState _ wd _)
  | length xs <= 1 = return state { output = unlines [ "mv: missing operands" ] }
  | otherwise      = ioHelper (mvHelper $ combine wd $ last xs) "mv" (init xs) state

mvHelper :: FilePath -> FilePath -> IO ()
mvHelper dest source = do
  fileExists <- doesFileExist dest
  directoryExists <- doesDirectoryExist dest
  contents <- getList source
  isFile <- doesFileExist source
  mvHelper' fileExists directoryExists isFile contents dest source

mvHelper' :: Bool -> Bool -> Bool -> [FilePath] -> FilePath -> FilePath -> IO ()
mvHelper' fileExists directoryExists isFile contents dest source
  | isFile && not directoryExists     = renameFile source dest
  | isFile && directoryExists         = renameFile source $ combine dest $ takeFileName source
  | not isFile && directoryExists     = mapM_ (\x -> mvHelper (combine dest x) (combine source x)) contents
  | not isFile && not directoryExists = createDirectory dest >> mvHelper dest source
  | otherwise                         = return ()
  where exists = fileExists || directoryExists

getList :: FilePath -> IO [FilePath]
getList path = do
  directoryExists <- doesDirectoryExist path
  contents <- if (directoryExists) then getDirectoryContents path else return []
  return $ filter (\x -> x /= "." && x /= "..") contents

cp :: Command
cp xs state@(ScriptState _ wd _)
  | length xs <= 1 = return state { output = unlines [ "cp: missing operands" ] }
  | otherwise      = ioHelper (cpHelper $ combine wd $ last xs) "cp" (init xs) state

cpHelper :: FilePath -> FilePath -> IO ()
cpHelper dest source = do
  directoryExists <- doesDirectoryExist dest
  let dest' = if (directoryExists) then combine dest $ takeFileName source
                                   else dest
  putStrLn $ unlines [source, " + ", dest']
  copyFile source dest'

rm :: Command
rm = ioHelper removeFile "rm"

cpdir :: Command
cpdir xs state@(ScriptState _ wd _)
  | length xs <= 1 = return state { output = unlines [ "cpdir: missing operands" ] }
  | otherwise      = ioHelper (cpdirHelper $ combine wd $ last xs) "cpdir" (init xs) state

cpdirHelper :: FilePath -> FilePath -> IO ()
cpdirHelper dest source = do
  contents <- getList source
  mapM_ (\x -> copyFile (combine source x) (combine dest x)) contents

mkdir :: Command
mkdir = ioHelper createDirectory "mkdir"

rmdir :: Command
rmdir = ioHelper removeDirectory "rmdir"

ioHelper :: (FilePath -> IO ()) -> String -> Command
ioHelper _ name [] state = return state { output = unlines[name ++ ": missing operand"] }
ioHelper f name xs state = ioActionHelper f name xs state

ioActionHelper :: (FilePath -> IO ()) -> String -> Command
ioActionHelper _ _ [] state = return state
ioActionHelper f name (x:xs) state@(ScriptState _ wd _) = do
  r <- try $ f $ combine wd x :: IO (Either IOException ())
  case r of
    Left e -> return state { output = unlines [ name ++ ": " ++ ioeGetErrorString e ] }
    Right val -> do state' <- ioActionHelper f name xs state
                    return state' { output = "" }

