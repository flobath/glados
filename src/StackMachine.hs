module StackMachine (
    Value(..),
    Operator(..),
    StackInstruction(..),
    Args,
    Stack,
    StackProgram,
    Environment,
    push,
    pop,
    pushEnv,
    storeEnv,
    applyOperator,
    addition,
    subtraction,
    multiplication,
    division,
    modulus,
    equal,
    notEqual,
    lessThan,
    lessEqual,
    greaterThan,
    greaterEqual,
    andOperator,
    orOperator,
    execute'
) where

import Data.Int (Int64)
import Data.Text (Text)

data Value = IntValue Int64 | BoolValue Bool deriving (Show, Eq)

data Operator = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge | And | Or deriving (Show, Eq)

data StackInstruction = PushValue Value | PushEnv String | StoreEnv String | Call Int | NewEnv | CallFuncName Text
    | Return | Jump Int | JumpIfFalse Int | OpValue Operator | Print Value
    deriving (Show, Eq)

type Args = [Value]

type Stack = [Value]

type StackProgram = [StackInstruction]

type Environment = [(String, Value)]

type ProgramCounter = Int

type ReturnStack = [Int]

-- Stack operations

push :: Value -> Stack -> Stack
push x stack = x : stack

pop :: Stack -> Either String (Value, Stack)
pop (x:xs) = Right (x, xs)
pop [] = Left "Cannot pop empty stack"

pushEnv :: String -> Environment -> Stack -> Either String Stack
pushEnv name env stack = case lookup name env of
    Just value -> Right (push value stack)
    Nothing -> Left "Cannot find value in environment"

storeEnv :: String -> Environment -> Stack -> Either String Environment
storeEnv name env (value : stack) =
    let updatedEnv = case lookup name env of
                        Just _  -> map (\(k, v) -> if k == name then (k, value) else (k, v)) env
                        Nothing -> (name, value) : env
    in Right updatedEnv
storeEnv _ _ [] = Left "Stack is empty"

-- Arithmetic operators

applyOperator :: Operator -> Stack -> Either String Stack
applyOperator Add = addition
applyOperator Sub = subtraction
applyOperator Mul = multiplication
applyOperator Div = division
applyOperator Mod = modulus
applyOperator Eq = equal
applyOperator Ne = notEqual
applyOperator Lt = lessThan
applyOperator Le = lessEqual
applyOperator Gt = greaterThan
applyOperator Ge = greaterEqual
applyOperator And = andOperator
applyOperator Or = orOperator

addition :: Stack -> Either String Stack
addition (IntValue x : IntValue y : xs) = Right (IntValue (x + y) : xs)
addition _ = Left "Cannot apply addition"

subtraction :: Stack -> Either String Stack
subtraction (IntValue x : IntValue y : xs) = Right (IntValue (x - y) : xs)
subtraction _ = Left "Cannot apply subtraction"

multiplication :: Stack -> Either String Stack
multiplication (IntValue x : IntValue y : xs) = Right (IntValue (x * y) : xs)
multiplication _ = Left "Cannot apply multiplication"

division :: Stack -> Either String Stack
division (IntValue _ : IntValue 0 : _) = Left "Division by zero"
division (IntValue x : IntValue y : xs) = Right (IntValue (x `div` y) : xs)
division _ = Left "Cannot apply division"

modulus :: Stack -> Either String Stack
modulus (IntValue _ : IntValue 0 : _) = Left "Modulus by zero"
modulus (IntValue x : IntValue y : xs) = Right (IntValue (x `mod` y) : xs)
modulus _ = Left "Cannot apply modulus"

-- Comparison operators

equal :: Stack -> Either String Stack
equal (IntValue x : IntValue y : xs) = Right (BoolValue (x == y) : xs)
equal (BoolValue x : BoolValue y : xs) = Right (BoolValue (x == y) : xs)
equal _ = Left "Cannot apply equal"

notEqual :: Stack -> Either String Stack
notEqual (IntValue x : IntValue y : xs) = Right (BoolValue (x /= y) : xs)
notEqual (BoolValue x : BoolValue y : xs) = Right (BoolValue (x /= y) : xs)
notEqual _ = Left "Cannot apply not equal"

lessThan :: Stack -> Either String Stack
lessThan (IntValue x : IntValue y : xs) = Right (BoolValue (x < y) : xs)
lessThan _ = Left "Cannot apply less than"

lessEqual :: Stack -> Either String Stack
lessEqual (IntValue x : IntValue y : xs) = Right (BoolValue (x <= y) : xs)
lessEqual _ = Left "Cannot apply less equal"

greaterThan :: Stack -> Either String Stack
greaterThan (IntValue x : IntValue y : xs) = Right (BoolValue (x > y) : xs)
greaterThan _ = Left "Cannot apply greater than"

greaterEqual :: Stack -> Either String Stack
greaterEqual (IntValue x : IntValue y : xs) = Right (BoolValue (x >= y) : xs)
greaterEqual _ = Left "Cannot apply greater equal"

andOperator :: Stack -> Either String Stack
andOperator (BoolValue x : BoolValue y : xs) = Right (BoolValue (x && y) : xs)
andOperator _ = Left "Cannot apply and"

orOperator :: Stack -> Either String Stack
orOperator (BoolValue x : BoolValue y : xs) = Right (BoolValue (x || y) : xs)
orOperator _ = Left "Cannot apply or"

-- Print

boolToString :: Bool -> String
boolToString True = "true"
boolToString False = "false"

intToString :: Int64 -> String
intToString = show

printFunc :: Stack -> Either String String
printFunc (x : xs) = case x of
    IntValue n -> Right (intToString n)
    BoolValue b -> Right (boolToString b)
printFunc _ = Left "Cannot print"

-- Execution

execute' :: StackProgram -> Either String (String, Value)
execute' prog = execute [[]] [] prog 0 [] []

execute :: [Environment] -> Args -> StackProgram -> ProgramCounter -> ReturnStack -> Stack -> Either String (String, Value)
execute _ _ [] _ _ stack = case pop stack of
    Right (value, _) -> Right ("", value)
    Left err -> Left err
execute envStack args prog pc returnStack stack
    | pc >= length prog = Left "Program counter out of bounds"
    | otherwise = case prog !! pc of
        Return -> case returnStack of
            (retAddr:rest) -> execute (tail envStack) args prog retAddr rest stack
            [] -> case pop stack of
                Right (value, _) -> Right ("", value)
                Left err -> Left err
        PushValue x -> execute envStack args prog (pc + 1) returnStack (push x stack)
        PushEnv name -> case pushEnv name (head envStack) stack of
            Right stack' -> execute envStack args prog (pc + 1) returnStack stack'
            Left err -> Left err
        StoreEnv name -> case storeEnv name (head envStack) stack of
            Right env' -> execute (env':tail envStack) args prog (pc + 1) returnStack stack
            Left err -> Left err
        NewEnv -> execute ([]:envStack) args prog (pc + 1) returnStack stack
        Call n -> execute envStack args prog n (pc + 1 : returnStack) stack
        CallFuncName name -> Left "Should not happen"
        OpValue op -> case applyOperator op stack of
            Left err -> Left err
            Right stack' -> execute envStack args prog (pc + 1) returnStack stack'
        Jump n -> execute envStack args prog (pc + n) returnStack stack
        JumpIfFalse n -> case stack of
            (BoolValue False : _) -> execute envStack args prog (pc + n) returnStack stack
            _ -> execute envStack args prog (pc + 1) returnStack stack
        Print x -> case execute envStack args prog (pc + 1) returnStack stack of
            Right (output, value) -> case printFunc (x : stack) of
                Right str -> Right (output ++ str, value)
                Left err -> Left err
            Left error -> Left error
        _ -> Left "Execution error"
