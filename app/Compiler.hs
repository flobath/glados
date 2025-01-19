{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler (compiler) where

import Parser.ParseAndLex (parseAndLexFile)
import Parser (pProgram)
import GldsBytecode (writeProgramToFile)
import ConvertASTtoInstructions (convertToStackInstructions)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import StackMachine(StackProgram)
import Helpers((>&<), orelse, headOr, orExitWith, exitWithErrorMessage)

compileFile :: FilePath -> T.Text -> Either String StackProgram
compileFile filename input = do
    ast <- parseAndLexFile filename pProgram input >&< show
    convertToStackInstructions ast

compiler :: [String] -> IO ()
compiler args = do
    file <- headOr args `orExitWith` "No input file to compile"
    contents <- T.IO.readFile file
    instrs <- compileFile file contents `orelse` exitWithErrorMessage
    writeProgramToFile "a.out" instrs
