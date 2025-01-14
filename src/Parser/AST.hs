module Parser.AST (
    Program(..),
    MainFunction(..),
    Function(..),
    Statement(..),
    Expression(..),
    BlockExpression(..),
    Operation(..),
    PrefixOperation(..),
    InfixOperation(..),
    AtomicExpression(..),
    VarIdentifier(..),
    TypeIdentifier(..),
    VariableDeclaration(..),
    PrefixOperator,
    InfixOperator,
) where

import Data.Text (Text)
import Data.Int (Int64)

data Program = Program MainFunction [Function]
    deriving (Eq, Show)

data MainFunction = MainFunction [VariableDeclaration] BlockExpression
    deriving (Eq, Show)
data Function = Function Text [VariableDeclaration] (Maybe TypeIdentifier) BlockExpression
    deriving (Eq, Show)

newtype BlockExpression = BlockExpression [Statement]
    deriving (Eq, Show)

data Statement
    = StExpression Expression
    | StVariableDecl VariableDeclaration (Maybe Expression)
    | StAssignment VarIdentifier Expression
    | StReturn Expression
    deriving (Eq, Show)

data Expression
    = ExprIfConditional Expression Expression (Maybe Expression)
    | ExprFunctionCall Expression [Expression]
    | ExprOperation Operation
    | ExprBlock BlockExpression
    | ExprAtomic AtomicExpression
    deriving (Eq, Show)

data Operation
    = OpPrefix PrefixOperation
    | OpInfix  InfixOperation
    deriving (Eq, Show)

data PrefixOperation
    = PreNot  Expression
    | PrePlus Expression
    | PreNeg  Expression
    deriving (Eq, Show)
type PrefixOperator = (Expression -> PrefixOperation)

data InfixOperation
    = InfixAdd  Expression Expression
    | InfixSub  Expression Expression
    | InfixMul  Expression Expression
    | InfixDiv  Expression Expression
    | InfixMod  Expression Expression
    | InfixEq   Expression Expression
    | InfixNeq  Expression Expression
    | InfixGt   Expression Expression
    | InfixGe   Expression Expression
    | InfixLt   Expression Expression
    | InfixLe   Expression Expression
    | InfixAnd  Expression Expression
    | InfixOr   Expression Expression
    deriving (Eq, Show)
type InfixOperator = (Expression -> Expression -> InfixOperation)

data AtomicExpression
    = AtomIntLiteral Int64
    | AtomBooleanLiteral Bool
    | AtomIdentifier VarIdentifier
    deriving (Eq, Show)

newtype VarIdentifier = VarIdentifier Text
    deriving (Eq, Show)
newtype TypeIdentifier = TypeIdentifier Text
    deriving (Eq, Show)

data VariableDeclaration = VariableDeclaration TypeIdentifier VarIdentifier
    deriving (Eq, Show)
