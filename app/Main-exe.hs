{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Control.Monad.Trans.State (StateT(runStateT))
-- import ParseAndLex (parseAndLex)
import System.Environment (getArgs)

main :: IO ()
main = do
    -- args <- getArgs
    -- if null args
    --     then putStrLn "No file found"
    --     else do
    --         let file = head args
    --         contents <- readFile file
    --         print contents
    return ()
