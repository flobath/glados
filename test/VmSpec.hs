{-# LANGUAGE OverloadedStrings #-}
module VmSpec (spec) where

import Test.Hspec ( hspec, describe, it, shouldBe , shouldThrow, anyException, Spec)
import StackMachine ( push, pop, pushEnv,
    applyOperator, execute',
    subtraction, multiplication, division, modulus, addition,
    equal, notEqual, lessThan, lessEqual, greaterThan, greaterEqual, andOperator, orOperator,
    Value(..), Operator(..), StackInstruction(..), StackProgram)
import Control.Exception (evaluate)
import qualified Data.Text
import Data.Text (Text)

testText :: Text -> (Text, Int)
testText t = (t, Data.Text.length t)

spec :: Spec
spec = do
    describe "operators" $ do
        it "subtraction 2 - 1" $ do
            subtraction [IntValue 2, IntValue 1] `shouldBe` Right [IntValue 1]
        it "subtraction 1 - 2" $ do
            subtraction [IntValue 1, IntValue 2] `shouldBe` Right [IntValue (-1)]
        it "substraction 1 - 2 , 3" $ do
            subtraction [IntValue 1, IntValue 2, IntValue 3] `shouldBe` Right [IntValue (-1), IntValue 3]
        it "substraction error" $ do
            subtraction [IntValue 1] `shouldBe` Left "Cannot apply subtraction"
        it "multiplication 2 * 3" $ do
            multiplication [IntValue 2, IntValue 3] `shouldBe` Right [IntValue 6]
        it "multiplication 2 * 3 , 4" $ do
            multiplication [IntValue 2, IntValue 3, IntValue 4] `shouldBe` Right [IntValue 6, IntValue 4]
        it "multiplication error" $ do
            multiplication [IntValue 2] `shouldBe` Left "Cannot apply multiplication"
        it "division 6 / 3" $ do
            division [IntValue 6, IntValue 3] `shouldBe` Right [IntValue 2]
        it "division 6 / 3 , 2" $ do
            division [IntValue 6, IntValue 3, IntValue 2] `shouldBe` Right [IntValue 2, IntValue 2]
        it "division error" $ do
            division [IntValue 6] `shouldBe` Left "Cannot apply division"
        it "division by zero" $ do
            division [IntValue 6, IntValue 0] `shouldBe` Left "Division by zero"
        it "modulus 6 % 2" $ do
            modulus [IntValue 6, IntValue 3] `shouldBe` Right [IntValue 0]
        it "modulus 6 % 5" $ do
            modulus [IntValue 6, IntValue 5] `shouldBe` Right [IntValue 1]
        it "modulus 6 % 3 , 2" $ do
            modulus [IntValue 6, IntValue 3, IntValue 2] `shouldBe` Right [IntValue 0, IntValue 2]
        it "modulus error" $ do
            modulus [IntValue 6] `shouldBe` Left "Cannot apply modulus"
        it "modulus by zero" $ do
            modulus [IntValue 6, IntValue 0] `shouldBe` Left "Modulus by zero"
        it "addition 1 + 2" $ do
            addition [IntValue 1, IntValue 2] `shouldBe` Right [IntValue 3]
        it "addition 1 + 2 , 3" $ do
            addition [IntValue 1, IntValue 2, IntValue 3] `shouldBe` Right [IntValue 3, IntValue 3]
        it "addition error" $ do
            addition [IntValue 1] `shouldBe` Left "Cannot apply addition"
        it "equal 1 == 1" $ do
            equal [IntValue 1, IntValue 1] `shouldBe` Right [BoolValue True]
        it "equal 1 == 2" $ do
            equal [IntValue 1, IntValue 2] `shouldBe` Right [BoolValue False]
        it "equal 1 == 2 , 3" $ do
            equal [IntValue 1, IntValue 2, IntValue 3] `shouldBe` Right [BoolValue False, IntValue 3]
        it "equal error" $ do
            equal [IntValue 1] `shouldBe` Left "Cannot apply equal"
        it "notEqual 1 /= 1" $ do
            notEqual [IntValue 1, IntValue 1] `shouldBe` Right [BoolValue False]
        it "notEqual 1 /= 2" $ do
            notEqual [IntValue 1, IntValue 2] `shouldBe` Right [BoolValue True]
        it "notEqual 1 /= 2 , 3" $ do
            notEqual [IntValue 1, IntValue 2, IntValue 3] `shouldBe` Right [BoolValue True, IntValue 3]
        it "notEqual error" $ do
            notEqual [IntValue 1] `shouldBe` Left "Cannot apply not equal"
        it "lessThan 1 < 2" $ do
            lessThan [IntValue 1, IntValue 2] `shouldBe` Right [BoolValue True]
        it "lessThan 2 < 1" $ do
            lessThan [IntValue 2, IntValue 1] `shouldBe` Right [BoolValue False]
        it "lessThan 1 < 1" $ do
            lessThan [IntValue 1, IntValue 1] `shouldBe` Right [BoolValue False]
        it "lessThan 1 < 2 , 3" $ do
            lessThan [IntValue 1, IntValue 2, IntValue 3] `shouldBe` Right [BoolValue True, IntValue 3]
        it "lessEqual error" $ do
            lessThan [IntValue 1] `shouldBe` Left "Cannot apply less than"
        it "lessEqual 1 <= 2" $ do
            lessEqual [IntValue 1, IntValue 2] `shouldBe` Right [BoolValue True]
        it "lessEqual 2 <= 1" $ do
            lessEqual [IntValue 2, IntValue 1] `shouldBe` Right [BoolValue False]
        it "lessEqual 1 <= 1" $ do
            lessEqual [IntValue 1, IntValue 1] `shouldBe` Right [BoolValue True]
        it "lessEqual 1 <= 2 , 3" $ do
            lessEqual [IntValue 1, IntValue 2, IntValue 3] `shouldBe` Right [BoolValue True, IntValue 3]
        it "lessEqual error" $ do
            lessEqual [IntValue 1] `shouldBe` Left "Cannot apply less equal"
        it "greaterThan 1 > 2" $ do
            greaterThan [IntValue 1, IntValue 2] `shouldBe` Right [BoolValue False]
        it "greaterThan 2 > 1" $ do
            greaterThan [IntValue 2, IntValue 1] `shouldBe` Right [BoolValue True]
        it "greaterThan 1 > 1" $ do
            greaterThan [IntValue 1, IntValue 1] `shouldBe` Right [BoolValue False]
        it "greaterThan 1 > 2 , 3" $ do
            greaterThan [IntValue 1, IntValue 2, IntValue 3] `shouldBe` Right [BoolValue False, IntValue 3]
        it "greaterThan error" $ do
            greaterThan [IntValue 1] `shouldBe` Left "Cannot apply greater than"
        it "greaterEqual 1 >= 2" $ do
            greaterEqual [IntValue 1, IntValue 2] `shouldBe` Right [BoolValue False]
        it "greaterEqual 2 >= 1" $ do
            greaterEqual [IntValue 2, IntValue 1] `shouldBe` Right [BoolValue True]
        it "greaterEqual 1 >= 1" $ do
            greaterEqual [IntValue 1, IntValue 1] `shouldBe` Right [BoolValue True]
        it "greaterEqual 1 >= 2 , 3" $ do
            greaterEqual [IntValue 1, IntValue 2, IntValue 3] `shouldBe` Right [BoolValue False, IntValue 3]
        it "greaterEqual error" $ do
            greaterEqual [IntValue 1] `shouldBe` Left "Cannot apply greater equal"
        it "andOperator T && T" $ do
            andOperator [BoolValue True, BoolValue True] `shouldBe` Right [BoolValue True]
        it "andOperator T && F" $ do
            andOperator [BoolValue True, BoolValue False] `shouldBe` Right [BoolValue False]
        it "andOperator F && F" $ do
            andOperator [BoolValue False, BoolValue False] `shouldBe` Right [BoolValue False]
        it "andOperator T && F , T" $ do
            andOperator [BoolValue True, BoolValue False, BoolValue True] `shouldBe` Right [BoolValue False, BoolValue True]
        it "andOperator error" $ do
            andOperator [BoolValue True] `shouldBe` Left "Cannot apply and"
        it "orOperator T || T" $ do
            orOperator [BoolValue True, BoolValue True] `shouldBe` Right [BoolValue True]
        it "orOperator T || F" $ do
            orOperator [BoolValue True, BoolValue False] `shouldBe` Right [BoolValue True]
        it "orOperator F || F" $ do
            orOperator [BoolValue False, BoolValue False] `shouldBe` Right [BoolValue False]
        it "orOperator T || F , T" $ do
            orOperator [BoolValue True, BoolValue False, BoolValue True] `shouldBe` Right [BoolValue True, BoolValue True]
        it "orOperator error" $ do
            orOperator [BoolValue True] `shouldBe` Left "Cannot apply or"
    --describe "jump" $ do
    --    it "jumpIfTrue 2 True" $ do
    --        jumpIfTrue 2 [BoolValue True] [PushValue (IntValue 1), PushValue (IntValue 2), PushValue (IntValue 3)] `shouldBe` [PushValue (IntValue 3)]
    --    it "jumpIfTrue 2 False" $ do
    --        jumpIfTrue 2 [BoolValue False] [PushValue (IntValue 1), PushValue (IntValue 2), PushValue (IntValue 3)] `shouldBe` [PushValue (IntValue 1), PushValue (IntValue 2), PushValue (IntValue 3)]
    --    it "jumpIfFalse 2 True" $ do
    --        jumpIfFalse 2 [BoolValue True] [PushValue (IntValue 1), PushValue (IntValue 2), PushValue (IntValue 3)] `shouldBe` [PushValue (IntValue 1), PushValue (IntValue 2), PushValue (IntValue 3)]
    --    it "jumpIfFalse 2 False" $ do
    --        jumpIfFalse 2 [BoolValue False] [PushValue (IntValue 1), PushValue (IntValue 2), PushValue (IntValue 3)] `shouldBe` [PushValue (IntValue 3)]
    describe "push" $ do
        it "push 1" $ do
            push (IntValue 1) [] `shouldBe` [IntValue 1]
        it "push 1, 2" $ do
            push (IntValue 1) [IntValue 2] `shouldBe` [IntValue 1, IntValue 2]
    describe "pop" $ do
        it "pop 1" $ do
            pop [IntValue 1] `shouldBe` Right (IntValue 1, [])
        it "pop 1, 2" $ do
            pop [IntValue 1, IntValue 2] `shouldBe` Right (IntValue 1, [IntValue 2])
    --describe "pushArgs" $ do
    --    it "pushArgs 2" $ do
    --        pushArgs 2 [IntValue 1, IntValue 2, IntValue 3] [IntValue 1, IntValue 2] `shouldBe` Right [IntValue 3, IntValue 1, IntValue 2]
    --    it "pushArgs 0, 1" $ do
    --        pushArgs 0 [IntValue 1] [IntValue 2, IntValue 3] `shouldBe` Right [IntValue 1, IntValue 2, IntValue 3]
    describe "pushEnv" $ do
        it "pushEnv" $ do
            pushEnv "env" [("env", IntValue 8)] [] `shouldBe` Right [IntValue 8]
    describe "applyOperation" $ do
        it "applyOperator Add" $ do
            applyOperator Add [IntValue 1, IntValue 2] `shouldBe` Right [IntValue 3]
        it "applyOperator Sub" $ do
            applyOperator Sub [IntValue 1, IntValue 2] `shouldBe` Right [IntValue (-1)]
        it "applyOperator Mul" $ do
            applyOperator Mul [IntValue 2, IntValue 3] `shouldBe` Right [IntValue 6]
        it "applyOperator Div" $ do
            applyOperator Div [IntValue 6, IntValue 3] `shouldBe` Right [IntValue 2]
        it "applyOperator Mod" $ do
            applyOperator Mod [IntValue 6, IntValue 3] `shouldBe` Right [IntValue 0]
        it "applyOperator Eq" $ do
            applyOperator Eq [IntValue 1, IntValue 1] `shouldBe` Right [BoolValue True]
        it "applyOperator Ne" $ do
            applyOperator Ne [IntValue 1, IntValue 1] `shouldBe` Right [BoolValue False]
        it "applyOperator Lt" $ do
            applyOperator Lt [IntValue 1, IntValue 2] `shouldBe` Right [BoolValue True]
        it "applyOperator Le" $ do
            applyOperator Le [IntValue 1, IntValue 2] `shouldBe` Right [BoolValue True]
        it "applyOperator Gt" $ do
            applyOperator Gt [IntValue 1, IntValue 2] `shouldBe` Right [BoolValue False]
        it "applyOperator Ge" $ do
            applyOperator Ge [IntValue 1, IntValue 2] `shouldBe` Right [BoolValue False]
        it "applyOperator And" $ do
            applyOperator And [BoolValue True, BoolValue True] `shouldBe` Right [BoolValue True]
        it "applyOperator Or" $ do
            applyOperator Or [BoolValue True, BoolValue False] `shouldBe` Right [BoolValue True]
    describe "execute" $ do
        it "execute" $ do
            execute' [PushValue (IntValue 1), PushValue (IntValue 2), OpValue Add, Return] `shouldBe` Right (IntValue 3)
        it "execute addition error" $ do
            execute' [PushValue (IntValue 1), OpValue Add, Return] `shouldBe` Left "Cannot apply addition"
        it "execute subtraction error" $ do
            execute' [PushValue (IntValue 1), OpValue Sub, Return] `shouldBe` Left "Cannot apply subtraction"
        it "execute multiplication error" $ do
            execute' [PushValue (IntValue 1), OpValue Mul, Return] `shouldBe` Left "Cannot apply multiplication"
        it "execute division error" $ do
            execute' [PushValue (IntValue 1), OpValue Div, Return] `shouldBe` Left "Cannot apply division"
        it "execute modulus error" $ do
            execute' [PushValue (IntValue 1), OpValue Mod, Return] `shouldBe` Left "Cannot apply modulus"
        it "execute equal error" $ do
            execute' [PushValue (IntValue 1), OpValue Eq, Return] `shouldBe` Left "Cannot apply equal"
        it "execute notEqual error" $ do
            execute' [PushValue (IntValue 1), OpValue Ne, Return] `shouldBe` Left "Cannot apply not equal"
        it "execute lessThan error" $ do
            execute' [PushValue (IntValue 1), OpValue Lt, Return] `shouldBe` Left "Cannot apply less than"
        it "execute lessEqual error" $ do
            execute' [PushValue (IntValue 1), OpValue Le, Return] `shouldBe` Left "Cannot apply less equal"
        it "execute greaterThan error" $ do
            execute' [PushValue (IntValue 1), OpValue Gt, Return] `shouldBe` Left "Cannot apply greater than"
        it "execute greaterEqual error" $ do
            execute' [PushValue (IntValue 1), OpValue Ge, Return] `shouldBe` Left "Cannot apply greater equal"
        it "execute and error" $ do
            execute' [PushValue (BoolValue True), OpValue And, Return] `shouldBe` Left "Cannot apply and"
        it "execute or error" $ do
            execute' [PushValue (BoolValue True), OpValue Or, Return] `shouldBe` Left "Cannot apply or"

        it "execute while loop" $ do
            execute' [
                    PushValue (IntValue 5),
                    StoreEnv "a",
                    PushValue (IntValue 7),
                    PushEnv "a",
                    OpValue Lt,
                    JumpIfFalse 6,
                    PushValue (IntValue 1),
                    PushEnv "a",
                    OpValue Add,
                    StoreEnv "a",
                    Jump (-8),
                    PushEnv "a",
                    Return
                ]
            `shouldBe` Right (IntValue 7)
        it "execute until loop" $ do
            execute' [
                    PushValue (IntValue 10),
                    StoreEnv "a",
                    PushValue (IntValue 7),
                    PushEnv "a",
                    OpValue Lt,
                    PushValue (BoolValue False),
                    OpValue Eq,
                    JumpIfFalse 6,
                    PushValue (IntValue 1),
                    PushEnv "a",
                    OpValue Sub,
                    StoreEnv "a",
                    Jump (-10),
                    PushEnv "a",
                    Return
                ]
            `shouldBe` Right (IntValue 6)
        it "execute do while loop" $ do
            execute' [
                    PushValue (IntValue 5),
                    StoreEnv "a",
                    PushValue (IntValue 1),
                    PushEnv "a",
                    OpValue Add,
                    StoreEnv "a",
                    PushValue (IntValue 7),
                    PushEnv "a",
                    OpValue Lt,
                    JumpIfFalse 2,
                    Jump (-8),
                    PushEnv "a",
                    Return
                ]
            `shouldBe` Right (IntValue 7)
        it "execute for loop" $ do
            execute' [
                    PushValue (IntValue 0),
                    StoreEnv "a",
                    PushValue (IntValue 0),
                    StoreEnv "i",
                    PushValue (IntValue 5),
                    PushEnv "i",
                    OpValue Lt,
                    JumpIfFalse 10,
                    PushEnv "i",
                    PushEnv "a",
                    OpValue Add,
                    StoreEnv "a",
                    PushValue (IntValue 1),
                    PushEnv "i",
                    OpValue Add,
                    StoreEnv "i",
                    Jump (-12),
                    PushEnv "a",
                    Return
                ]
            `shouldBe` Right (IntValue 10)
