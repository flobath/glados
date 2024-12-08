{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lib (
    Environment(..),
    Expression(..),
    Primitive(..),
    Symbol(..),
    Operator(..),
    Arguments(..),
    evaluate,
    symbolRef,
    symbolList,
    constant,
    boolean,
    text,
    --void,
    defaultEnvironment,
    oppositeOperator,
    addOperator,
    subtractOperator,
    multiplyOperator,
    divideOperator,
    moduloOperator,
    notOperator,
    eqOperator,
    neqOperator,
    inferiorOperator,
    ifOperator,
    callOperator,
    defineOperator,
    lambdaOperator,
) where

import Data.Functor ((<&>))

newtype Symbol = Symbol String
    deriving Eq

instance Show Symbol where
    show (Symbol name) = name

data Primitive = Constant Int
    | Boolean Bool
    | Text String
    -- | Void
    | SymbolReference Symbol
    | SymbolList [Symbol]
    | Data [(Symbol, Primitive)]
    | Function [Symbol] Expression
    deriving Eq

instance Show Primitive where
    show (Constant value) = show value
    show (Boolean value) = show value
    show (Text value) = '"' : value ++ "\""
    --show Void = "Void"
    show (SymbolReference symbol) = '[' : show symbol ++ "]"
    show (SymbolList syms) = '(' : unwords (map show syms) ++ ")"
    show (Data pairs) = '{' : unwords (map (\(key, value) -> show key ++ ": " ++ show value) pairs) ++ "}"
    show (Function params body) = "(Function with paramaters {" ++ unwords (map show params) ++ "} evaluating " ++ show body ++ ")"

data Arguments = Single Expression
    | Pair Expression Expression
    | Triple Expression Expression Expression
    | List [Expression]
    deriving Eq

data Operator = Unary String | Binary String | Ternary String | Nary String
    deriving Eq

instance Show Operator where
    show (Unary name) = "unary operator '" ++ name ++ "'"
    show (Binary name) = "binary operator '" ++ name ++ "'"
    show (Ternary name) = "ternary operator '" ++ name ++ "'"
    show (Nary "") = "call operator"
    show (Nary name) = "n-ary operator '" ++ name ++ "'"

data Expression = Primitive Primitive | Operation Operator Arguments
    deriving Eq

instance Show Expression where
    show (Primitive atom) = show atom
    show (Operation operator (Single expr)) = "(Applying " ++ show operator ++ " to " ++ show expr ++ ")"
    show (Operation operator (Pair left right)) = "(Applying " ++ show operator ++ " to " ++ show left ++ " and " ++ show right ++ ")"
    show (Operation operator (Triple first second third)) = "(Applying " ++ show operator ++ " to " ++ show first ++ " and " ++ show second ++ " and " ++ show third ++ ")"
    show (Operation operator (List exprs)) = "(Applying " ++ show operator ++ " to arguments " ++ unwords (map show exprs) ++ ")"

data Environment = Environment {
    operators :: [(Operator, Environment -> Arguments -> Either (Environment, Expression) Environment)],
    symbols :: [(Symbol, Expression)]
}

instance Show Environment where
    show (Environment ops syms) = "Environment with " ++ show (length ops) ++ " operators defined and the following " ++ show (length syms) ++ " symbols " ++ show syms

instance Eq Environment where
    (==) (Environment _ syms1) (Environment _ syms2) = syms1 == syms2


symbolRef = Primitive . SymbolReference
symbolList = Primitive . SymbolList
constant = Primitive . Constant
boolean = Primitive . Boolean
text = Primitive . Text
--void = Primitive Void

oppositeOperator = Unary "-"
opposite env (Single (evaluate env -> Left (env', Primitive (Constant x)))) = Left (env', constant (-x))
opposite env _ = Right env

addOperator = Binary "+"
add env (Pair (evaluate env -> Left (_, Primitive (Constant a))) (evaluate env -> Left (_, Primitive (Constant b)))) = Left (env, constant $ a + b)
add env _ = Right env

subtractOperator = Binary "-"
subtract env (Pair (evaluate env -> Left (_, Primitive (Constant a))) (evaluate env -> Left (_, Primitive (Constant b)))) = Left (env, constant $ a - b)
subtract env _ = Right env

multiplyOperator = Binary "*"
multiply env (Pair (evaluate env -> Left (_, Primitive (Constant a))) (evaluate env -> Left (_, Primitive (Constant b)))) = Left (env, constant $ a * b)
multiply env _ = Right env

divideOperator = Binary "/"
divide env (Pair _ (evaluate env -> Left (_, Primitive (Constant 0)))) = Right env
divide env (Pair (evaluate env -> Left (_, Primitive (Constant a))) (evaluate env -> Left (_, Primitive (Constant b)))) = Left (env, constant $ a `div` b)
divide env _ = Right env

moduloOperator = Binary "%"
modulo env (Pair _ (evaluate env -> Left (_, Primitive (Constant 0)))) = Right env
modulo env (Pair (evaluate env -> Left (_, Primitive (Constant a))) (evaluate env -> Left (_, Primitive (Constant b)))) = Left (env, constant $ a `mod` b)
modulo env _ = Right env

notOperator = Unary "!"
notOperation env (Single (evaluate env -> Left (_, Primitive (Boolean b)))) = Left (env, boolean $ not b)
notOperation env _ = Right env

eqOperator = Binary "=="
eq env (Pair (evaluate env -> Left (_, a)) (evaluate env -> Left (_, b))) = Left (env, boolean $ a == b)
eq env _ = Right env

neqOperator = Binary "!="
neq env (Pair (evaluate env -> Left (_, a)) (evaluate env -> Left (_, b))) = Left (env, boolean $ a /= b)
neq env _ = Right env

inferiorOperator = Binary "<"
inferior env (Pair (evaluate env -> Left (_, Primitive (Constant a))) (evaluate env -> Left (_, Primitive (Constant b)))) = Left (env, boolean $ a < b)
inferior env _ = Right env

ifOperator = Ternary "if"
ifOperation env (Triple (evaluate env -> Left (env', Primitive (Boolean True))) (evaluate env' -> Left (_, result)) _) = Left (env, result)
ifOperation env (Triple (evaluate env -> Left (env', Primitive (Boolean False))) _ (evaluate env' -> Left (_, result))) = Left (env, result)
ifOperation env _ = Right env

addAndEvalSymbol env@(Environment ops syms) (symbol, evaluate env -> Left (_, expr)) = Environment ops ((symbol, expr) : syms)
addAndEvalSymbol env _ = env

callOperator = Nary ""
call env (List ((evaluate env -> Left (env', Primitive (Function params body))) : (zipStrict params -> Just pairs))) =
    case evaluate (foldl addAndEvalSymbol env' pairs) body of
        Left (_, result) -> Left (env, result)
        Right _ -> Right env
call env _ = Right env

defineOperator = Binary "define"
define env (Pair (Primitive (SymbolList [name])) value) = evaluate (addSymbol env (name, value)) value
define env (Pair (Primitive (SymbolList (name:params))) value) = let function = Primitive $ Function params value in
    Left (addSymbol env (name, function), function)
define env _ = Right env

lambdaOperator = Binary "lambda"
lambda env (Pair (Primitive (SymbolList params)) body) = Left (env, Primitive $ Function params body)
lambda env _ = Right env

defaultOperators = [
    (oppositeOperator, opposite),
    (addOperator, add),
    (subtractOperator, Lib.subtract),
    (multiplyOperator, multiply),
    (divideOperator, divide),
    (moduloOperator, modulo),
    (notOperator, notOperation),
    (eqOperator, eq),
    (neqOperator, neq),
    (inferiorOperator, inferior),
    (ifOperator, ifOperation),
    (callOperator, call),
    (defineOperator, define),
    (lambdaOperator, lambda)
    ]

defaultEnvironment = Environment defaultOperators []

zipStrict [] [] = Just []
zipStrict [] _ = Nothing
zipStrict _ [] = Nothing
zipStrict (x:xs) (y:ys) = zipStrict xs ys <&> ((x, y):)

dereference env symbol = lookup symbol $ symbols env

addSymbol (Environment ops syms) symbol = Environment ops (symbol : syms)

-- SymbolReference Primitives are pass-through
evaluate env (Primitive (SymbolReference (dereference env -> Just expr))) = evaluate env expr
evaluate env (Primitive (SymbolReference (dereference env -> Nothing))) = Right env
-- All other Primitives evaluate to themselves
evaluate env (Primitive p) = Left (env, Primitive p)
evaluate env (Operation operator args) =
    case lookup operator $ operators env of
        Just op -> case op env args of
            Left (env', result) -> Left (env', result)
            Right env' -> Right env'
        Nothing -> Right env
