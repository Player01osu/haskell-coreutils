import Control.Monad (liftM)
import System.Environment (getArgs)
import System.IO (readFile)
import Text.Printf (printf)

help =
  unlines
    [ "Usage: tac [OPTION]... [FILE]..."
    , "Write each FILE to standard output, last line first."
    , ""
    , "With no FILE, or when FILE is -, read standard input."
    , ""
    , "Mandatory arguments to long options are mandatory for short options too."
    , "  -b, --before             attach the separator before instead of after"
    , "  -r, --regex              interpret the separator as a regular expression"
    , "  -s, --separator=STRING   use STRING as the separator instead of newline"
    , "      --help        display this help and exit"
    , "      --version     output version information and exit"
    ]

data Content =
  Content
    { contentFlags :: [Flag]
    , contentFiles :: [String]
    }
  deriving (Show)

addFlag :: Content -> Flag -> Content
addFlag content flag =
  Content (flag : contentFlags content) (contentFiles content)

addFile :: Content -> String -> Content
addFile content file =
  Content (contentFlags content) (file : contentFiles content)

data Flag
  = FlagNumber
  | FlagShowEnds
  | FlagNumberAll
  | FlagSqueeze
  | FlagShowTabs
  deriving (Show)

parseLongFlag = undefined

parseFlag :: Content -> String -> [String] -> IO String
parseFlag content [] args = parseArgs content args
parseFlag content ('A':xs) args = undefined
parseFlag content ('b':xs) args = parseArgs (addFlag content FlagNumber) args
parseFlag content ('e':xs) args = undefined
parseFlag content ('E':xs) args = parseArgs (addFlag content FlagShowEnds) args
parseFlag content ('n':xs) args = parseArgs (addFlag content FlagNumberAll) args
parseFlag content ('s':xs) args = parseArgs (addFlag content FlagSqueeze) args
parseFlag content ('t':xs) args = undefined
parseFlag content ('T':xs) args = parseArgs (addFlag content FlagShowTabs) args
parseFlag content ('u':xs) args = undefined
parseFlag content ('v':xs) args = undefined
parseFlag content (c:xs) args = error $ printf "cat: invalid option -- '%s'" c

getFile :: String -> IO String
getFile "-" = getContents
getFile file = readFile file

getFileReverse :: String -> IO String
getFileReverse file = liftM (unlines . reverse . lines) $ getFile file

tac :: Content -> IO String
tac (Content flags []) = getContents
tac (Content flags files) =
  liftM unlines $ sequence $ map getFileReverse $ reverse files

parseArgs :: Content -> [String] -> IO String
parseArgs content ["-h"] = return help
parseArgs content [] = tac content
parseArgs content ("-":xs) = parseArgs (addFile content "-") xs
parseArgs content (('-':'-':a):xs) = parseLongFlag content a xs
parseArgs content (('-':a):xs) = parseFlag content a xs
parseArgs content (file:xs) = parseArgs (addFile content file) xs

main = getArgs >>= parseArgs (Content [] []) >>= putStr
