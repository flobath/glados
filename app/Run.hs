module Run (run) where
import Helpers (exitWithErrorMessage, orelse)
import qualified Data.Text.IO as T.IO
import Compiler (compileFile)
import StackMachine (execute')


run :: [String] -> IO ()
run [inputFile] = do
    contents <- T.IO.readFile inputFile
    program <- compileFile contents `orelse` exitWithErrorMessage
    result <- execute' program `orelse` exitWithErrorMessage
    print result
run _ = exitWithErrorMessage "Expecting a single file to run"
