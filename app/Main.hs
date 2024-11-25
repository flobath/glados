{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Lib

nSymbol = Symbol "n"
factorialSymbol = Symbol "factorial"
functionDefinition = Operation defineOperator $ Pair (symbolList [factorialSymbol, nSymbol]) $
    Operation ifOperator $ Triple (Operation eqOperator $ Pair (symbolRef nSymbol) (constant 0))
        (constant 1)
        (Operation multiplyOperator $ Pair
            (Operation callOperator $ List [symbolRef factorialSymbol,
                Operation substractOperator $ Pair (symbolRef nSymbol) (constant 1)
            ])
            (symbolRef nSymbol))

main :: IO ()
main = case evaluate defaultEnvironment functionDefinition of
    Left (env, expr) -> print $ evaluate env $ Operation callOperator $ List [expr, constant 6]
    Right errEnv -> print errEnv
