module Lexer.Tokens (
    Token(..),
    Literal(..),
    Keyword (..),
    ControlSequence(..),
) where

import Data.Int (Int64)
import Data.Text (Text)

data Token
    = Keyword Keyword
    | Literal Literal
    | Identifier Text
    | Control ControlSequence
    deriving (Show, Eq, Ord)

-- we use 'newtype' instead of data because we only have one variant so far
newtype Literal
    = IntLiteral Int64
    deriving (Show, Eq, Ord)

data Keyword
    = KeyWReturn
    | KeyWIf
    | KeyWUnless
    | KeyWElse
    | KeyWWhile
    | KeyWUntil
    | KeyWDo
    | KeyWFun
    | KeyWTrue
    | KeyWFalse
    | KeyWMain
    deriving (Show, Eq, Ord)

data ControlSequence
    = LineBreak
    | Semicolon
    | Colon
    | Comma
    | OpenParen
    | CloseParen
    | OpenBrace
    | CloseBrace
    | OperAssign
    | OperAdd
    | OperSub
    | OperMul
    | OperDiv
    | OperMod
    | OperEquals
    | OperDiffer
    | OperNot
    | OperAnd
    | OperOr
    | OperGt
    | OperLt
    | OperGe
    | OperLe
    deriving (Show, Eq, Ord)
