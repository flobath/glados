{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import System.Environment (getArgs)
import VM(vm)

main :: IO ()
main = vm =<< getArgs
