{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GldsBytecode(readProgramFromFile)
import StackMachine (execute')
import System.Environment (getArgs)
import Helpers (headOr, orExitWith, exitWithErrorMessage, orelse)

main :: IO ()
main = do
    args <- getArgs
    file <- headOr args `orExitWith` "No input file to compile"
    eitherProgram <- readProgramFromFile file
    program <- eitherProgram `orelse` exitWithErrorMessage
    case execute' program of
        Left err -> putStrLn err
        Right (print, return) -> putStrLn print

