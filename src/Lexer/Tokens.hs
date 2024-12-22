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

-- we use 'newtype' instead of data because we only have one variant so far
newtype Literal
    = IntLiteral Int64

data Keyword
    = KeyWReturn
    | KeyWIf
    | KeyWUnless
    | KeyWElse
    | KeyWFun
    | KeyWTrue
    | KeyWFalse
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
