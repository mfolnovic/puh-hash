module Language.Commands (commands) where

import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import qualified Data.Map as M
import Language.Exec (Command, ScriptState(..))

import Numeric (showHex)

import System.Console.GetOpt
import System.Directory
import System.Posix.Files
import System.IO
import System.IO.Error
import System.FilePath

import Text.Printf (printf)

-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands :: M.Map String Command
commands = M.fromList [ ("echo", echo)
                      , ("cat", cat)
                      , ("ls", ls)
                      , ("pwd", pwd)
                      , ("cd", cd)
                      , ("create", create)
                      , ("mv", mv)
                      , ("cp", cp)
                      , ("rm", rm)
                      , ("cpdir", cpdir)
                      , ("mkdir", mkdir)
                      , ("rmdir", rmdir)
                      , ("grep", grep)
                      , ("chmod", chmod)
                      , ("hexdump", hexdump)
                      ]

echo :: Command
echo x _ state = return $ state { output = unlines [intercalate " " x] }

cat :: Command
cat [] h state = do
  content <- readHandle h
  return state { output = content }
cat xs _ state@(ScriptState _ wd _) = do
  files <- sequence $ map (safeRead . combine wd) xs
  return state { output = unlines files }

safeRead :: FilePath -> IO String
safeRead path = do
  fileExists <- doesFileExist path
  directoryExists <- doesDirectoryExist path
  case (fileExists, directoryExists) of
    (True, _) -> readFile path
    (False, True) -> return $ "cat: " ++ path ++ " is a directory."
    (False, False) -> return $ "cat: " ++ path ++ " does not exist."

ls :: Command
ls xs _ state@(ScriptState _ wd _) = do
  let paths = if null xs then [wd] else xs
  files <- sequence $ map (getList . combine wd) paths
  let lists = map (unlines . map takeFileName) files
  return state { output = unlines lists }

pwd :: Command
pwd _ _ state = do
  return state { output = unlines [wd state] }

cd :: Command
cd xs _ state@(ScriptState _ wd _) = do
  homeDir <- getHomeDirectory
  path <- if null xs then getHomeDirectory
                     else return $ next $ reverse $ map dropTrailingPathSeparator $ splitPath $ head xs
  fileExists <- doesFileExist path
  directoryExists <- doesDirectoryExist path
  case (fileExists, directoryExists) of
    (_, True) -> return state { output = "", wd = path }
    (True, False) -> return state { output = unlines ["cd: " ++ (head xs) ++ " is a file."] }
    (False, False) -> return state { output = unlines ["cd: " ++ (head xs) ++ " does not exist."] }
  where next [] = wd
        next (".":xs) = next xs
        next ("..":xs) = takeDirectory $ next xs
        next (dir:xs) = combine (next xs) dir

create :: Command
create xs _ state@(ScriptState _ wd _) = do
  let paths = map (combine wd) xs
  handles <- sequence $ map (\x -> openFile x AppendMode) paths
  return state { output = "" }

mv :: Command
mv xs h state@(ScriptState _ wd _)
  | length xs <= 1 = return state { output = unlines [ "mv: missing operands" ] }
  | otherwise      = ioHelper (mvHelper $ combine wd $ last xs) "mv" (init xs) h state

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
  return $ sort $ filter (\x -> x /= "." && x /= "..") contents

cp :: Command
cp xs h state@(ScriptState _ wd _)
  | length xs <= 1 = return state { output = unlines [ "cp: missing operands" ] }
  | otherwise      = ioHelper (cpHelper $ combine wd $ last xs) "cp" (init xs) h state

cpHelper :: FilePath -> FilePath -> IO ()
cpHelper dest source = do
  directoryExists <- doesDirectoryExist dest
  let dest' = if (directoryExists) then combine dest $ takeFileName source
                                   else dest
  copyFile source dest'

rm :: Command
rm = ioHelper removeFile "rm"

cpdir :: Command
cpdir xs h state@(ScriptState _ wd _)
  | length xs <= 1 = return state { output = unlines [ "cpdir: missing operands" ] }
  | otherwise      = ioHelper (cpdirHelper $ combine wd $ last xs) "cpdir" (init xs) h state

cpdirHelper :: FilePath -> FilePath -> IO ()
cpdirHelper dest source = do
  contents <- getList source
  mapM_ (\x -> copyFile (combine source x) (combine dest x)) contents

mkdir :: Command
mkdir = ioHelper createDirectory "mkdir"

rmdir :: Command
rmdir = ioHelper removeDirectory "rmdir"

data GrepFlag = Invert | IgnoreCase | OnlyMatching | LineNumber | Count deriving (Eq)

grepOptions :: [OptDescr GrepFlag]
grepOptions = [ Option ['v'] ["invert-match"]  (NoArg Invert)       "Invert the sense of matching, to select non-matching lines."
              , Option ['i'] ["ignore-case"]   (NoArg IgnoreCase)   "Ignore case distinctions in both the pattern and the input file."
              , Option ['o'] ["only-matching"] (NoArg OnlyMatching) "Print only the matched (non-empty) parts of a matching line."
              , Option ['n'] ["line-number"]   (NoArg LineNumber)   "Prefix each line of output with the line number."
              , Option ['c'] ["count"]         (NoArg Count)        "Print only a count of matching line for each input file."
              ]

grep :: Command
grep xs h state = case getOpt Permute grepOptions xs of
  (flags, args, []) -> if (length args < 1) then return state { output = header }
                                            else grepHelper h state flags args
  (_, _, errors)    -> return state { output = concat errors ++ header }
  where header = usageInfo "Usage: grep [OPTION...] pattern files..." grepOptions

grepHelper :: Handle -> ScriptState -> [GrepFlag] -> [String] -> IO ScriptState
grepHelper h state flags [pattern] = do
  content <- readHandle h
  let output = grepString state flags pattern content
  return state { output = output }
grepHelper _ state flags [pattern, file] = do
  output <- grepFile state flags pattern file
  return state { output = output }
grepHelper _ state flags (pattern:xs) = do
  results <- forM xs $ grepFile state flags pattern
  let output = unlines $ concat [[file ++ ":", result] | (file, result) <- zip xs results]
  return state { output = output }

grepFile :: ScriptState -> [GrepFlag] -> String -> String -> IO String
grepFile state flags pattern file = withFile file ReadMode $ \h -> do
  contents <- hGetContents h
  return $ grepString state flags pattern contents

grepString :: ScriptState -> [GrepFlag] -> String -> String -> String
grepString state flags pattern content =
  unlines $ grepMap flags pattern $ filter (grepFilter flags pattern) xs
  where xs = zip [1..] $ lines content

grepFilter :: [GrepFlag] -> String -> (Int, String) -> Bool
grepFilter flags pattern = invert . ignoreCase . snd
  where ignoreCase str = isInfixOf (lowerStr pattern) (lowerStr str)
        invert x = if Invert `elem` flags then not x else x
        lowerStr str = if IgnoreCase `elem` flags then map toLower str
                                                  else str

grepMap :: [GrepFlag] -> String -> [(Int, String)] -> [String]
grepMap flags pattern = count . onlyMatching . lineNumber
  where lineNumber results = if LineNumber `elem` flags then [show i ++ ": " ++ x | (i, x) <- results]
                                                        else map snd results
        onlyMatching results = if OnlyMatching `elem` flags then replicate (length results) pattern
                                                            else results
        count results = if Count `elem` flags then [show $ length results]
                                              else results

data ChmodFlag = ChmodFlag { references :: String
                           , operator   :: Char
                           , modes      :: String } deriving Show

chmod :: Command
chmod xs h state@(ScriptState _ wd _)
  | length xs <= 1         = return state { output = unlines [ "chmod: missing operands" ] }
  | (operator flag) == 'u' = return state { output = unlines [ "chmod: invalid flags" ] }
  | (null $ modes flag)    = return state { output = unlines [ "chmod: invalid flags" ] }
  | otherwise              = ioHelper (chmodHelper flag) "chmod" (tail xs) h state
  where flag = parseFlag $ head xs

parseFlag :: String -> ChmodFlag
parseFlag flags = ChmodFlag references' operator modes
  where references = takeWhile (\x -> x `notElem` ['+', '-', '=']) flags
        references' = if null references then "a" else references
        afterRef = drop (length references) flags
        operator = if null afterRef then 'u' else head afterRef
        modes = if null afterRef then "" else tail afterRef

chmodHelper :: ChmodFlag -> FilePath -> IO ()
chmodHelper flag path = do
  current <- fileMode <$> getFileStatus path
  let mask = foldl (.|.) 0 allModes
  let next = case (operator flag) of
               '+' -> current .|. mask
               '-' -> current .&. (complement mask)
               '=' -> mask
  setFileMode path next
  where read = 'r' `elem` modes flag
        write = 'w' `elem` modes flag
        execute = 'x' `elem` modes flag
        user = all || 'u' `elem` references flag
        group = all || 'g' `elem` references flag
        others = all || 'o' `elem` references flag
        all = 'a' `elem` references flag
        availableModes = [read, write, execute]
        availableRefs = [user, group, others]
        userModes =  filterModes [ownerReadMode, ownerWriteMode, ownerExecuteMode]
        groupModes = filterModes [groupReadMode, groupWriteMode, groupExecuteMode]
        otherModes = filterModes [otherReadMode, otherWriteMode, otherExecuteMode]
        allModes = concat $ map fst $ filter (\x -> snd x == True) $ zip [userModes, groupModes, otherModes] availableRefs 
        filterModes xs = map fst $ filter (\x -> snd x == True) $ zip xs availableModes

hexdump :: Command
hexdump [] h state = hexdumpHelper state <$> BSC.pack <$> readHandle h
hexdump [path] _ state = hexdumpHelper state <$> BS.readFile path
hexdump _ _ state = return state { output = unlines [ "hexdump: only one file" ] }

hexdumpHelper :: ScriptState -> BS.ByteString -> ScriptState
hexdumpHelper state@(ScriptState _ wd _) content = state { output = output }
  where output = unlines $ final 0 $ grouping $ BS.unpack content
        grouping [] = []
        grouping [x] = [pad 4 $ "0" ++ (showHex x "")]
        grouping (x:y:xs) = pad 4 ((showHex y "") ++ (showHex x "")) : grouping xs
        final _ [] = [index (BS.length content) ++ ""]
        final i xs = (index i ++ " " ++ intercalate " " ys) : final (i + 16) zs
          where (ys, zs) = splitAt 8 xs
        index x = pad 7 $ showHex x ""
        pad n x = printf ("%0" ++ show n ++ "s") x

readHandle :: Handle -> IO String
readHandle h = do
  r <- try $ hGetChar h
  case r of
    Left e       -> if (isEOFError e) then return "" else return ""
    Right '\CAN' -> return ""
    Right next   -> do buf <- readHandle h
                       return $ next : buf

ioHelper :: (FilePath -> IO ()) -> String -> Command
ioHelper _ name [] _ state = return state { output = unlines[name ++ ": missing operand"] }
ioHelper f name xs h state = ioActionHelper f name xs h state

ioActionHelper :: (FilePath -> IO ()) -> String -> Command
ioActionHelper _ _ [] _ state = return state
ioActionHelper f name (x:xs) h state@(ScriptState _ wd _) = do
  r <- try $ f $ combine wd x :: IO (Either IOException ())
  case r of
    Left e -> return state { output = unlines [ name ++ ": " ++ ioeGetErrorString e ] }
    Right val -> do state' <- ioActionHelper f name xs h state
                    return state' { output = "" }
