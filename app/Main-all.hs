module Main (main) where

import System.Environment(getArgs)
import VM(vm)
import Compiler(compiler)
import System.Exit (exitWith, ExitCode (ExitFailure))

helpMessage :: String
helpMessage = "FunChill combined toolkit\n\nAvailable commands are:\n\
               \- build:    Produce an executable from source files\n\
               \- exec:     Execute a previously produced executable\n"

mainCombined :: [String] -> IO ()
mainCombined ("build":args) = compiler args
mainCombined ("exec":args) = vm args
mainCombined ("--help":_) = putStrLn helpMessage
mainCombined _ = mainCombined ["--help"] >> exitWith (ExitFailure 84)

main :: IO ()
main = mainCombined =<< getArgs
