module Lexer.Tokens (
    Token(..),
    Literal(..),
    Keyword (..),
    ControlSequence(..),
) where

import Data.Int (Int64)
import Data.Text (Text)
import Text.Megaparsec (SourcePos)

data WithPos a = WithPos
    { startPos :: SourcePos
    , endPos :: SourcePos
    , tokenLength :: Int
    , tokenVal :: a
    } deriving (Eq, Ord, Show)

data TokenStream = TokenStream
    { myStreamInput :: String
    , unMyStream :: [WithPos Token]
    }


data Token
    = Keyword Keyword
    | Literal Literal
    | Identifier Text
    | Control ControlSequence
    deriving (Show, Eq)

-- we use 'newtype' instead of data because we only have one variant so far
newtype Literal
    = IntLiteral Int64
    deriving (Show, Eq)

data Keyword
    = KeyWReturn
    | KeyWIf
    | KeyWUnless
    | KeyWElse
    | KeyWFun
    | KeyWTrue
    | KeyWFalse
    | KeyWMain
    deriving (Show, Eq)

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
    deriving (Show, Eq)
