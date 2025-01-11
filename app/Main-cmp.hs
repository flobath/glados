{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Control.Monad.Trans.State (StateT(runStateT))
import Parser.ParseAndLex (parseAndLex, ParseLexError)
import Parser2 (pProgram)
import GldsBytecode (writeProgramToFile)
import ConvertASTtoInstructions (convertToStackInstructions)
import System.Environment (getArgs)
import qualified Data.Text as T

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No file found"
        else do
            let file = head args
            contents <- readFile file
            let contentsText = T.pack contents
            case parseAndLex pProgram contentsText of
                Left err -> print err 
                Right ast -> case convertToStackInstructions ast of
                    instrs -> writeProgramToFile "a.out" instrs
