{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Repl (replLoop, ReplState (ReplState), ifTTY)
import Control.Monad.Trans.State (StateT(runStateT))

nSymbol = Symbol "n"
factorialSymbol = Symbol "factorial"
functionDefinition = Operation defineOperator $ Pair (symbolList [factorialSymbol, nSymbol]) $
    Operation ifOperator $ Triple (Operation eqOperator $ Pair (symbolRef nSymbol) (constant 0))
        (constant 1)
        (Operation multiplyOperator $ Pair
            (Operation callOperator $ List [symbolRef factorialSymbol,
                Operation subtractOperator $ Pair (symbolRef nSymbol) (constant 1)
            ])
            (symbolRef nSymbol))

main :: IO ()
main = do
    ifTTY $ putStrLn "================="
    ifTTY $ putStrLn "  Glados scheme"
    ifTTY $ putStrLn "================="
    ifTTY $ putStrLn ""
    _ <- runStateT replLoop (ReplState "" defaultEnvironment)
    return ()
