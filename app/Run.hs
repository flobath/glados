module Run (run) where
import Helpers (exitWithErrorMessage, orelse)
import Data.Text(Text)
import qualified Data.Text.IO as T.IO
import Compiler (compileFile)
import StackMachine (execute')
import System.Exit (exitSuccess)

runHelpMessage :: String
runHelpMessage  = "Usage: run {filename | -}\n\n"
                ++ "Run a FunChill program directly from source code\n"
                ++ "Input can be a file, or stdin with \"-\" as filename\n"

runFromContent :: Text -> IO ()
runFromContent contents = do
    program <- compileFile contents `orelse` exitWithErrorMessage
    result <- execute' program `orelse` exitWithErrorMessage
    print result

run :: [String] -> IO ()
run ["--help"] = putStrLn runHelpMessage >> exitSuccess
run ["-h"] = run ["--help"]
run ["-"] = do
    contents <- T.IO.getContents
    runFromContent contents
run [inputFile] = do
    contents <- T.IO.readFile inputFile
    runFromContent contents
run _ = exitWithErrorMessage runHelpMessage
