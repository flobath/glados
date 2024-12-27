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
    PrimitiveExpression(..),
    VarIdentifier(..),
    TypeIdentifier(..),
    VariableDeclaration(..),
) where

import Data.Text (Text)
import Data.Int (Int64)

data Program = Program MainFunction [Function]

data MainFunction = MainFunction [VariableDeclaration] BlockExpression
data Function = Function Text [VariableDeclaration] BlockExpression

newtype BlockExpression = BlockExpression [Statement]

data Statement
    = StExpression Expression
    | StVariableDecl VariableDeclaration (Maybe Expression)
    | StReturn Expression

data Expression
    = ExprIfConditional Expression Expression (Maybe Expression)
    | ExprFunctionCall Expression [Expression]
    | ExprOperation Operation
    | ExprBlock BlockExpression
    | ExprPrimitive PrimitiveExpression

data Operation
    = OpPrefix PrefixOperation
    | OpInfix  InfixOperation

data PrefixOperation
    = PreNot  Expression
    | PrePlus Expression
    | PreNeg  Expression

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

data PrimitiveExpression
    = PrimIntLiteral Int64
    | PrimBooleanLiteral Bool
    | PrimIdentifier VarIdentifier

newtype VarIdentifier = VarIdentifier Text
newtype TypeIdentifier = TypeIdentifier Text

data VariableDeclaration = VariableDeclaration TypeIdentifier VarIdentifier
