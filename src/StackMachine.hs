module StackMachine (
    Value(..),
    Operator(..),
    Instruction(..),
    Args,
    Stack,
    Program,
    Environment,
    push,
    pop,
    pushArgs,
    pushEnv,
    applyOperator,
    jumpIfTrue,
    jumpIfFalse,
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
    execute
) where

data Value = IntValue Int | BoolValue Bool | FuncValue [Instruction]
    deriving (Show, Eq)

data Operator = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge | And | Or
    deriving (Show, Eq)

data Instruction = Push Value | PushArgs Int | PushEnv String | Call | Return | JumpIfTrue Int | JumpIfFalse Int | OpValue Operator
    deriving (Show, Eq)

type Args = [Value]

type Stack = [Value]

type Program = [Instruction]

type Environment = [(String, Value)]

-- Stack operations

push :: Value -> Stack -> Stack
push x stack = x : stack

pop :: Stack -> Either String (Value, Stack)
pop (x:xs) = Right (x, xs)
pop [] = Left "Cannot pop empty stack"

pushArgs :: Int -> Args -> Stack -> Either String Stack
pushArgs n args stack = case (n, args) of
    (0, (x:_)) -> Right (push x stack)
    (a, (_:xs)) -> pushArgs (a-1) xs stack
    (_, []) -> Left "Not enough arguments"

pushEnv :: String -> Environment -> Stack -> Either String Stack
pushEnv name env stack = case lookup name env of

    Just value -> Right (push value stack)
    Nothing -> Left "Cannot find value in environment"

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
addition _ = Left "Addition failed"

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

-- Control flow

jumpIfTrue :: Int -> Stack -> Program -> Program
jumpIfTrue n (BoolValue True : _) program = drop n program
jumpIfTrue _ _ program = program

jumpIfFalse :: Int -> Stack -> Program -> Program
jumpIfFalse n (BoolValue False : _) program = drop n program
jumpIfFalse _ _ program = program

-- Execution

getValueFromPop :: Either String (Value, Stack) -> Either String Value
getValueFromPop (Right (value, _)) = Right value
getValueFromPop (Left error) = Left error

execute :: Environment -> Args -> Program -> Stack -> Either String Value
-- execute _ _ [] stack = let (result, _) = case pop stack of
--         Right (value, _) -> (Right value)
--         Left err -> Left err
execute env args (Return : _) stack = execute env args [] stack

execute env args (Push x : prog') stack = execute env args prog' (push x stack)
execute env args ((PushArgs n) : prog') stack = case pushArgs n args stack of
    Right stack' -> execute env args prog' stack'
    Left err -> Left err
execute env args ((PushEnv name) : prog') stack = case pushEnv name env stack of
    Right stack' -> execute env args prog' stack'
    Left err -> Left err

execute env args (Call : prog') stack = case stack of
    (FuncValue func : stack') -> case execute env stack' func [] of
        Left err -> Left err
        Right value -> execute env args prog' (value : stack')
    _ -> Left "Cannot call"
execute env args ((OpValue op) : prog') stack = case applyOperator op stack of
    Left err -> Left err
    Right stack' -> execute env args prog' stack'

execute env args (JumpIfTrue n : prog') stack = execute env args (jumpIfTrue n stack prog') stack
execute env args (JumpIfFalse n : prog') stack = execute env args (jumpIfFalse n stack prog') stack
