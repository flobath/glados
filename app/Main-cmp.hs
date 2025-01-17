module Main (main) where

import System.Environment (getArgs)
import Compiler(compiler)

main :: IO ()
main = compiler =<< getArgs
