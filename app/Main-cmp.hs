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
import Helpers((>&<), orelse, headOr, orExitWith, exitWithErrorMessage)

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