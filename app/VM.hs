{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module VM (vm) where

import GldsBytecode(readProgramFromFile)
import StackMachine (execute')
import Helpers (headOr, orExitWith, exitWithErrorMessage, orelse)

vm :: [String] -> IO ()
vm args = do
    file <- headOr args `orExitWith` "No program to execute"
    eitherProgram <- readProgramFromFile file
    program <- eitherProgram `orelse` exitWithErrorMessage
    result <- execute' program `orelse` exitWithErrorMessage

    print result
