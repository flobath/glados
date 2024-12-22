module Tokens (
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

data Literal
    = IntLiteral Int64
    | BoolLiteral Bool

data Keyword
    = KeyWReturn
    | KeyWIf
    | KeyWUnless
    | KeyWElse
    | KeyWFun
    | KeyWMain

data ControlSequence
    = LineBreak
    | SemiColon
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
