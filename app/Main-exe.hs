{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Control.Monad.Trans.State (StateT(runStateT))
import GldsBytecode(readProgramFromFile)
import StackMachine (Program, execute)
import System.Environment (getArgs)
import Data.Either (either)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No file found"
        else do
            let file = head args
            program <- readProgramFromFile file
            let result = execute [] [] program []
            either
                (\err -> putStrLn $ "Error: " ++ err)
                print
                result