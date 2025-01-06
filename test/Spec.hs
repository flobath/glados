{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec ( hspec, describe, it, shouldBe , shouldThrow, anyException)
import StackMachine ( push, pop, pushArgs, pushEnv, 
    applyOperator, execute,
    subtraction, multiplication, division, modulus, addition,
    equal, notEqual, lessThan, lessEqual, greaterThan, greaterEqual, andOperator, orOperator,
    jumpIfTrue, jumpIfFalse,
    Value(..), Operator(..), Instruction(..), Program, Stack )
import Control.Exception (evaluate)
import qualified Data.Text
import Data.Text (Text)

testText :: Text -> (Text, Int)
testText t = (t, Data.Text.length t)

main :: IO ()
main = hspec $ do
    describe "operators" $ do
        it "subtraction 2 - 1" $ do
            subtraction [IntValue 2, IntValue 1] `shouldBe` [IntValue 1]
        it "subtraction 1 - 2" $ do
            subtraction [IntValue 1, IntValue 2] `shouldBe` [IntValue (-1)]
        it "substraction 1 - 2 , 3" $ do
            subtraction [IntValue 1, IntValue 2, IntValue 3] `shouldBe` [IntValue (-1), IntValue 3]
        it "multiplication 2 * 3" $ do
            multiplication [IntValue 2, IntValue 3] `shouldBe` [IntValue 6]
        it "multiplication 2 * 3 , 4" $ do
            multiplication [IntValue 2, IntValue 3, IntValue 4] `shouldBe` [IntValue 6, IntValue 4]
        it "division 6 / 3" $ do
            division [IntValue 6, IntValue 3] `shouldBe` [IntValue 2]
        it "division 6 / 3 , 2" $ do
            division [IntValue 6, IntValue 3, IntValue 2] `shouldBe` [IntValue 2, IntValue 2]
        it "modulus 6 % 2" $ do
            modulus [IntValue 6, IntValue 3] `shouldBe` [IntValue 0]
        it "modulus 6 % 5" $ do
            modulus [IntValue 6, IntValue 5] `shouldBe` [IntValue 1]
        it "modulus 6 % 3 , 2" $ do
            modulus [IntValue 6, IntValue 3, IntValue 2] `shouldBe` [IntValue 0, IntValue 2]
        it "addition 1 + 2" $ do
            addition [IntValue 1, IntValue 2] `shouldBe` [IntValue 3]
        it "addition 1 + 2 , 3" $ do
            addition [IntValue 1, IntValue 2, IntValue 3] `shouldBe` [IntValue 3, IntValue 3]
        it "equal 1 == 1" $ do
            equal [IntValue 1, IntValue 1] `shouldBe` [BoolValue True]
        it "equal 1 == 2" $ do
            equal [IntValue 1, IntValue 2] `shouldBe` [BoolValue False]
        it "equal 1 == 2 , 3" $ do
            equal [IntValue 1, IntValue 2, IntValue 3] `shouldBe` [BoolValue False, IntValue 3]
        it "notEqual 1 /= 1" $ do
            notEqual [IntValue 1, IntValue 1] `shouldBe` [BoolValue False]
        it "notEqual 1 /= 2" $ do
            notEqual [IntValue 1, IntValue 2] `shouldBe` [BoolValue True]
        it "notEqual 1 /= 2 , 3" $ do
            notEqual [IntValue 1, IntValue 2, IntValue 3] `shouldBe` [BoolValue True, IntValue 3]
        it "lessThan 1 < 2" $ do
            lessThan [IntValue 1, IntValue 2] `shouldBe` [BoolValue True]
        it "lessThan 2 < 1" $ do
            lessThan [IntValue 2, IntValue 1] `shouldBe` [BoolValue False]
        it "lessThan 1 < 1" $ do
            lessThan [IntValue 1, IntValue 1] `shouldBe` [BoolValue False]
        it "lessThan 1 < 2 , 3" $ do
            lessThan [IntValue 1, IntValue 2, IntValue 3] `shouldBe` [BoolValue True, IntValue 3]
        it "lessEqual 1 <= 2" $ do
            lessEqual [IntValue 1, IntValue 2] `shouldBe` [BoolValue True]
        it "lessEqual 2 <= 1" $ do
            lessEqual [IntValue 2, IntValue 1] `shouldBe` [BoolValue False]
        it "lessEqual 1 <= 1" $ do
            lessEqual [IntValue 1, IntValue 1] `shouldBe` [BoolValue True]
        it "lessEqual 1 <= 2 , 3" $ do
            lessEqual [IntValue 1, IntValue 2, IntValue 3] `shouldBe` [BoolValue True, IntValue 3]
        it "greaterThan 1 > 2" $ do
            greaterThan [IntValue 1, IntValue 2] `shouldBe` [BoolValue False]
        it "greaterThan 2 > 1" $ do
            greaterThan [IntValue 2, IntValue 1] `shouldBe` [BoolValue True]
        it "greaterThan 1 > 1" $ do
            greaterThan [IntValue 1, IntValue 1] `shouldBe` [BoolValue False]
        it "greaterThan 1 > 2 , 3" $ do
            greaterThan [IntValue 1, IntValue 2, IntValue 3] `shouldBe` [BoolValue False, IntValue 3]
        it "greaterEqual 1 >= 2" $ do
            greaterEqual [IntValue 1, IntValue 2] `shouldBe` [BoolValue False]
        it "greaterEqual 2 >= 1" $ do
            greaterEqual [IntValue 2, IntValue 1] `shouldBe` [BoolValue True]
        it "greaterEqual 1 >= 1" $ do
            greaterEqual [IntValue 1, IntValue 1] `shouldBe` [BoolValue True]
        it "greaterEqual 1 >= 2 , 3" $ do
            greaterEqual [IntValue 1, IntValue 2, IntValue 3] `shouldBe` [BoolValue False, IntValue 3]
        it "andOperator T && T" $ do
            andOperator [BoolValue True, BoolValue True] `shouldBe` [BoolValue True]
        it "andOperator T && F" $ do
            andOperator [BoolValue True, BoolValue False] `shouldBe` [BoolValue False]
        it "andOperator F && F" $ do
            andOperator [BoolValue False, BoolValue False] `shouldBe` [BoolValue False]
        it "andOperator T && F , T" $ do
            andOperator [BoolValue True, BoolValue False, BoolValue True] `shouldBe` [BoolValue False, BoolValue True]
        it "orOperator T || T" $ do
            orOperator [BoolValue True, BoolValue True] `shouldBe` [BoolValue True]
        it "orOperator T || F" $ do
            orOperator [BoolValue True, BoolValue False] `shouldBe` [BoolValue True]
        it "orOperator F || F" $ do
            orOperator [BoolValue False, BoolValue False] `shouldBe` [BoolValue False]
        it "orOperator T || F , T" $ do
            orOperator [BoolValue True, BoolValue False, BoolValue True] `shouldBe` [BoolValue True, BoolValue True]
    describe "jump" $ do
        it "jumpIfTrue 2 True" $ do
            jumpIfTrue 2 [BoolValue True] [Push (IntValue 1), Push (IntValue 2), Push (IntValue 3)] `shouldBe` [Push (IntValue 3)]
        it "jumpIfTrue 2 False" $ do
            jumpIfTrue 2 [BoolValue False] [Push (IntValue 1), Push (IntValue 2), Push (IntValue 3)] `shouldBe` [Push (IntValue 1), Push (IntValue 2), Push (IntValue 3)]
        it "jumpIfFalse 2 True" $ do
            jumpIfFalse 2 [BoolValue True] [Push (IntValue 1), Push (IntValue 2), Push (IntValue 3)] `shouldBe` [Push (IntValue 1), Push (IntValue 2), Push (IntValue 3)]
        it "jumpIfFalse 2 False" $ do
            jumpIfFalse 2 [BoolValue False] [Push (IntValue 1), Push (IntValue 2), Push (IntValue 3)] `shouldBe` [Push (IntValue 3)]
    describe "push" $ do
        it "push 1" $ do
            push (IntValue 1) [] `shouldBe` [IntValue 1]
        it "push 1, 2" $ do
            push (IntValue 1) [IntValue 2] `shouldBe` [IntValue 1, IntValue 2]
    describe "pop" $ do
        it "pop 1" $ do
            pop [IntValue 1] `shouldBe` (IntValue 1, [])
        it "pop 1, 2" $ do
            pop [IntValue 1, IntValue 2] `shouldBe` (IntValue 1, [IntValue 2])
    describe "pushArgs" $ do
        it "pushArgs 2" $ do
            pushArgs 2 [IntValue 1, IntValue 2, IntValue 3] [IntValue 1, IntValue 2] `shouldBe` [IntValue 3, IntValue 1, IntValue 2]
        it "pushArgs 0, 1" $ do
            pushArgs 0 [IntValue 1] [IntValue 2, IntValue 3] `shouldBe` [IntValue 1, IntValue 2, IntValue 3]
    describe "pushEnv" $ do
        it "pushEnv" $ do
            pushEnv "env" [("env", IntValue 8)] [] `shouldBe` [IntValue 8]
    describe "applyOperation" $ do
        it "applyOperator Add" $ do
            applyOperator Add [IntValue 1, IntValue 2] `shouldBe` [IntValue 3]
        it "applyOperator Sub" $ do
            applyOperator Sub [IntValue 1, IntValue 2] `shouldBe` [IntValue (-1)]
        it "applyOperator Mul" $ do
            applyOperator Mul [IntValue 2, IntValue 3] `shouldBe` [IntValue 6]
        it "applyOperator Div" $ do
            applyOperator Div [IntValue 6, IntValue 3] `shouldBe` [IntValue 2]
        it "applyOperator Mod" $ do
            applyOperator Mod [IntValue 6, IntValue 3] `shouldBe` [IntValue 0]
        it "applyOperator Eq" $ do
            applyOperator Eq [IntValue 1, IntValue 1] `shouldBe` [BoolValue True]
        it "applyOperator Ne" $ do
            applyOperator Ne [IntValue 1, IntValue 1] `shouldBe` [BoolValue False]
        it "applyOperator Lt" $ do
            applyOperator Lt [IntValue 1, IntValue 2] `shouldBe` [BoolValue True]
        it "applyOperator Le" $ do
            applyOperator Le [IntValue 1, IntValue 2] `shouldBe` [BoolValue True]
        it "applyOperator Gt" $ do
            applyOperator Gt [IntValue 1, IntValue 2] `shouldBe` [BoolValue False]
        it "applyOperator Ge" $ do
            applyOperator Ge [IntValue 1, IntValue 2] `shouldBe` [BoolValue False]
        it "applyOperator And" $ do
            applyOperator And [BoolValue True, BoolValue True] `shouldBe` [BoolValue True]
        it "applyOperator Or" $ do
            applyOperator Or [BoolValue True, BoolValue False] `shouldBe` [BoolValue True]
    describe "execute" $ do
        it "execute" $ do
            execute [] [] [Push (IntValue 1), Push (IntValue 2), OpValue Add, Return] [] `shouldBe` (IntValue 3)