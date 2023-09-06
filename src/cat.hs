import System.Environment (getArgs)
import Control.Monad (liftM)
import System.IO (readFile)
import Text.Printf (printf)

help = unlines ["Usage: cat [OPTION]... [FILE]...",
                "Concatenate FILE(s) to standard output.",
                "",
                "With no FILE, or when FILE is -, read standard input.",
                "",
                "  -A, --show-all           equivalent to -vET",
                "  -b, --number-nonblank    number nonempty output lines, overrides -n",
                "  -e                       equivalent to -vE",
                "  -E, --show-ends          display $ at end of each line",
                "  -n, --number             number all output lines",
                "  -s, --squeeze-blank      suppress repeated empty output lines",
                "  -t                       equivalent to -vT",
                "  -T, --show-tabs          display TAB characters as ^I",
                "  -u                       (ignored)",
                "  -v, --show-nonprinting   use ^ and M- notation, except for LFD and TAB",
                "      --help        display this help and exit",
                "      --version     output version information and exit",
                "",
                "Examples:",
                "  cat f - g  Output f's contents, then standard input, then g's contents.",
                "  cat        Copy standard input to standard output."]

data Content = Content
               { contentFlags :: [Flag]
               , contentFiles :: [String]
               } deriving (Show)

addFlag :: Content -> Flag -> Content
addFlag content flag = Content (flag:contentFlags content) (contentFiles content)

addFile :: Content -> String -> Content
addFile content file = Content (contentFlags content) (file:contentFiles content)

data Flag = FlagNumber
          | FlagShowEnds
          | FlagNumberAll
          | FlagSqueeze
          | FlagShowTabs
          deriving (Show)

parseLongFlag = undefined

parseFlag :: Content -> String -> [String] -> IO String
parseFlag content [] args       = parseArgs content args
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
parseFlag content (c:xs) args   = error $ printf "cat: invalid option -- '%s'" c

getFile :: String -> IO String
getFile "-"  = getContents
getFile file = readFile file

cat :: Content -> IO String
cat (Content flags [])    = getContents
cat (Content flags files) = liftM unlines $ sequence $ map getFile $ reverse files

parseArgs :: Content -> [String] -> IO String
parseArgs content ["-h"]           = return help
parseArgs content []               = cat content
parseArgs content ("-":xs)         = parseArgs (addFile content "-") xs
parseArgs content (('-':'-':a):xs) = parseLongFlag content a xs
parseArgs content (('-':a):xs)     = parseFlag content a xs
parseArgs content (file:xs)        = parseArgs (addFile content file) xs

main = getArgs >>= parseArgs (Content [] []) >>= putStr
