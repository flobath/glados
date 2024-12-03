{-# LANGUAGE ViewPatterns #-}

module Main where

import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "Symbol" $ do
        it "should show a symbol correctly" $ do
            show (Symbol "x") `shouldBe` "x"

    describe "Primitive" $ do
        it "should show a constant correctly" $ do
            show (Constant 42) `shouldBe` "42"
        it "should show a boolean correctly" $ do
            show (Boolean True) `shouldBe` "True"
        it "should show a text correctly" $ do
            show (Text "hello") `shouldBe` "\"hello\""
        it "should show a symbol reference correctly" $ do
            show (SymbolReference (Symbol "x")) `shouldBe` "[x]"
        it "should show a symbol list correctly" $ do
            show (SymbolList [Symbol "x", Symbol "y"]) `shouldBe` "(x y)"
        it "should show a data pair correctly" $ do
            show (Data [(Symbol "x", Constant 42)]) `shouldBe` "{x: 42}"
        it "should show a function correctly" $ do
            show (Function [Symbol "x"] (Primitive (Constant 42))) `shouldBe` "(Function with paramaters {x} evaluating 42)"

    describe "Operator" $ do
        it "should show a unary operator correctly" $ do
            show (Unary "-") `shouldBe` "unary operator '-'"
        it "should show a binary operator correctly" $ do
            show (Binary "+") `shouldBe` "binary operator '+'"
        it "should show a ternary operator correctly" $ do
            show (Ternary "if") `shouldBe` "ternary operator 'if'"
        it "should show an n-ary operator correctly" $ do
            show (Nary "") `shouldBe` "call operator"
            show (Nary "apply") `shouldBe` "n-ary operator 'apply'"

    describe "Expression" $ do
        it "should show a primitive expression correctly" $ do
            show (Primitive (Constant 42)) `shouldBe` "42"
        it "should show an operation with a single argument correctly" $ do
            show (Operation (Unary "-") (Single (Primitive (Constant 42)))) `shouldBe` "(Applying unary operator '-' to 42)"
        it "should show an operation with a pair of arguments correctly" $ do
            show (Operation (Binary "+") (Pair (Primitive (Constant 1)) (Primitive (Constant 2)))) `shouldBe` "(Applying binary operator '+' to 1 and 2)"
        it "should show an operation with a list of arguments correctly" $ do
            show (Operation (Nary "apply") (List [Primitive (Constant 1), Primitive (Constant 2)])) `shouldBe` "(Applying n-ary operator 'apply' to arguments 1 2)"

    describe "Environment" $ do
        it "should show an environment correctly" $ do
            let env = Environment [] [(Symbol "x", Primitive (Constant 42))]
            show env `shouldBe` "Environment with 0 operators defined and the following 1 symbols [(x,42)]"

    describe "evaluate" $ do
        it "should evaluate a constant correctly" $ do
            let env = defaultEnvironment
            evaluate env (Primitive (Constant 42)) `shouldBe` Left (env, Primitive (Constant 42))
        it "should evaluate a symbol reference correctly" $ do
            let env = Environment [] [(Symbol "x", Primitive (Constant 42))]
            evaluate env (Primitive (SymbolReference (Symbol "x"))) `shouldBe` Left (env, Primitive (Constant 42))
        it "should evaluate an addition operation correctly" $ do
            let env = defaultEnvironment
            evaluate env (Operation addOperator (Pair (Primitive (Constant 1)) (Primitive (Constant 2)))) `shouldBe` Left (env, Primitive (Constant 3))
