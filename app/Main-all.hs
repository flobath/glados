module Main (main) where

import System.Environment(getArgs)
import VM(vm, disassembler)
import Compiler(compiler)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Run (run)

type Module = (String, ([String] -> IO (), String))

modules :: [Module]
modules =
    [ ("build",       (compiler,     "Produce an executable from source files"))
    , ("exec",        (vm,           "Execute a previously built executable"))
    , ("run",         (run,          "Compile and execute without producing an object file"))
    , ("disassemble", (disassembler, "Disassemble one or more executables"))
    ]

helpMessage :: String
helpMessage = "FunChill combined toolkit\n\nAvailable commands are:\n"
            ++ (unlines . map descMod) modules
    where
        descMod :: Module -> String
        descMod (modName, (_, desc)) = unwords
            [ "- " ++ modName ++ ":"
            , replicate (longest - length modName) ' '
            , desc
            ]
        longest = maximum (map (length . fst) modules)

mainCombined :: [String] -> IO ()
mainCombined ("--help":_) = putStrLn helpMessage
mainCombined (modName:args) = case lookup modName modules of
    Nothing     -> mainCombined ["--help"] >> exitWith (ExitFailure 84)
    Just (m, _) -> m args
mainCombined [] = mainCombined ["--help"] >> exitWith (ExitFailure 84)

main :: IO ()
main = mainCombined =<< getArgs
