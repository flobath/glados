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
    file <- headOr args `orExitWith` "No program to execute"
    eitherProgram <- readProgramFromFile file
    program <- eitherProgram `orelse` exitWithErrorMessage
    result <- execute' program `orelse` exitWithErrorMessage

    print result
