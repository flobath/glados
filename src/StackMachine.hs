module StackMachine (
    Value(..),
    Operator(..),
    Instruction(..),
    Args,
    Stack,
    Program,
    Environment
) where

data Value = IntValue Int | BoolValue Bool | OpValue Operator | FuncValue [Instruction]
    deriving (Show)

data Operator = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge
    deriving (Show)

data Instruction = Push Value | PushArgs Int | PushEnv String | Call | Return | JumpIfTrue Int | JumpIfFalse Int
    deriving (Show)

type Args = [Value]

type Stack = [Value]

type Program = [Instruction]

type Environment = [(String, Value)]
