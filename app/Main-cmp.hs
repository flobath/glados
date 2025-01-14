{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser.ParseAndLex (parseAndLex)
import Parser2 (pProgram)
import GldsBytecode (writeProgramToFile)
import ConvertASTtoInstructions (convertToStackInstructions)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import StackMachine(StackProgram)
import Helpers((>&<), orelse)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)

exitWithErrorMessage :: String -> IO a
exitWithErrorMessage msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

orExitWith :: (String -> Either String a) -> String -> IO a
orExitWith f msg = f msg `orelse` exitWithErrorMessage

headOr :: [a] -> b -> Either b a
headOr [] b = Left b
headOr (x:_) _ = Right x

compileFile :: T.Text -> Either String StackProgram
compileFile input = do
    ast <- parseAndLex pProgram input >&< show
    convertToStackInstructions ast

main :: IO ()
main = do
    args <- getArgs
    file <- headOr args `orExitWith` "No input file to compile"
    contents <- T.IO.readFile file
    instrs <- compileFile contents `orelse` exitWithErrorMessage
    writeProgramToFile "a.out" instrs
