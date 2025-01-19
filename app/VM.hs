{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module VM (vm, disassembler) where

import GldsBytecode(readProgramFromFile)
import StackMachine (execute', StackProgram)
import Helpers (headOr, orExitWith, exitWithErrorMessage, orelse, mapMToSnd, )
import Disassembly (disassembleFile)

getProgram :: FilePath -> IO StackProgram
getProgram filename = do
    eitherProg <- readProgramFromFile filename
    eitherProg `orelse` exitWithErrorMessage

vm :: [String] -> IO ()
vm args = do
    file <- headOr args `orExitWith` "No program to execute"
    program <- getProgram file
    result <- execute' program `orelse` exitWithErrorMessage

    print result

disassembler :: [String] -> IO ()
disassembler [] = exitWithErrorMessage "No input file to disassemble"
disassembler xs = do
    stacks <- mapMToSnd getProgram xs
    mapM_ (putStr . disassembleFile) stacks
