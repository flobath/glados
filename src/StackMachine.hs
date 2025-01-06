module StackMachine (
    Value(..),
    Operator(..),
    Instruction(..),
    Args,
    Stack,
    Program,
    Environment,
    execute
) where

data Value = IntValue Int | BoolValue Bool | FuncValue [Instruction]
    deriving (Show)

data Operator = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge | And | Or
    deriving (Show)

data Instruction = Push Value | PushArgs Int | PushEnv String | Call | Return | JumpIfTrue Int | JumpIfFalse Int | OpValue Operator
    deriving (Show)

type Args = [Value]

type Stack = [Value]

type Program = [Instruction]

type Environment = [(String, Value)]

-- Stack operations

push :: Value -> Stack -> Stack
push x stack = x : stack

pop :: Stack -> (Value, Stack)
pop (x:xs) = (x, xs)
pop [] = error "Cannot pop from an empty stack"

pushArgs :: Int -> Args -> Stack -> Stack
pushArgs n args stack = case (n, args) of
    (0, (x:_)) -> push x stack
    (a, (_:xs)) -> pushArgs (a-1) xs stack
    (_, []) -> error "Arguments index out of bounds"

pushEnv :: String -> Environment -> Stack -> Stack
pushEnv name env stack = case lookup name env of

    Just value -> push value stack
    Nothing -> error "Environment variable not found"

-- Arithmetic operators

applyOperator :: Operator -> Stack -> Stack
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

addition :: Stack -> Stack
addition (IntValue x : IntValue y : xs) = IntValue (x + y) : xs
addition _ = error "Cannot apply addition"

subtraction :: Stack -> Stack
subtraction (IntValue x : IntValue y : xs) = IntValue (x - y) : xs
subtraction _ = error "Cannot apply subtraction"

multiplication :: Stack -> Stack
multiplication (IntValue x : IntValue y : xs) = IntValue (x * y) : xs
multiplication _ = error "Cannot apply multiplication"

division :: Stack -> Stack
division (IntValue _ : IntValue 0 : _) = error "Division by zero"
division (IntValue x : IntValue y : xs) = IntValue (x `div` y) : xs
division _ = error "Cannot apply division"

modulus :: Stack -> Stack
modulus (IntValue _ : IntValue 0 : _) = error "Modulus by zero"
modulus (IntValue x : IntValue y : xs) = IntValue (x `mod` y) : xs
modulus _ = error "Cannot apply modulus"

-- Comparison operators

equal :: Stack -> Stack
equal (IntValue x : IntValue y : xs) = BoolValue (x == y) : xs
equal (BoolValue x : BoolValue y : xs) = BoolValue (x == y) : xs
equal _ = error "Cannot apply equal"

notEqual :: Stack -> Stack
notEqual (IntValue x : IntValue y : xs) = BoolValue (x /= y) : xs
notEqual (BoolValue x : BoolValue y : xs) = BoolValue (x /= y) : xs
notEqual _ = error "Cannot apply not equal"

lessThan :: Stack -> Stack
lessThan (IntValue x : IntValue y : xs) = BoolValue (x < y) : xs
lessThan _ = error "Cannot apply less than"

lessEqual :: Stack -> Stack
lessEqual (IntValue x : IntValue y : xs) = BoolValue (x <= y) : xs
lessEqual _ = error "Cannot apply less equal"

greaterThan :: Stack -> Stack
greaterThan (IntValue x : IntValue y : xs) = BoolValue (x > y) : xs
greaterThan _ = error "Cannot apply greater than"

greaterEqual :: Stack -> Stack
greaterEqual (IntValue x : IntValue y : xs) = BoolValue (x >= y) : xs
greaterEqual _ = error "Cannot apply greater equal"

andOperator :: Stack -> Stack
andOperator (BoolValue x : BoolValue y : xs) = BoolValue (x && y) : xs
andOperator _ = error "Cannot apply and"

orOperator :: Stack -> Stack
orOperator (BoolValue x : BoolValue y : xs) = BoolValue (x || y) : xs
orOperator _ = error "Cannot apply or"

-- Control flow

jumpIfTrue :: Int -> Stack -> Program -> Program
jumpIfTrue n (BoolValue True : _) program = drop n program
jumpIfTrue _ _ program = program

jumpIfFalse :: Int -> Stack -> Program -> Program
jumpIfFalse n (BoolValue False : _) program = drop n program
jumpIfFalse _ _ program = program

-- Execution

execute :: Environment -> Args -> Program -> Stack -> Value
execute _ _ [] stack = let (result, _) = pop stack in result
execute env args (Return : _) stack = execute env args [] stack

execute env args (Push x : prog') stack = execute env args prog' (push x stack)
execute env args ((PushArgs n) : prog') stack = execute env args prog' (pushArgs n args stack)
execute env args ((PushEnv name) : prog') stack = execute env args prog' (pushEnv name env stack)

execute env args (Call : prog') stack = case stack of
    (FuncValue func : stack') -> execute env args prog' (execute env stack' func [] : stack')
    _ -> error "Cannot call"
execute env args ((OpValue op) : prog') stack = execute env args prog' (applyOperator op stack)

execute env args ((JumpIfTrue n) : prog') stack = let (bool, stack') = pop stack in execute env args (jumpIfTrue n (bool:stack') prog') stack'
execute env args ((JumpIfFalse n) : prog') stack = let (bool, stack') = pop stack in execute env args (jumpIfFalse n (bool:stack') prog') stack'
