{
module Lexer (Token(..), AlexPosn(..), alexScanTokens, WithPos(..), myScanTokens) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Megaparsec(SourcePos(..), mkPos)
import Lexer.Tokens (Token(..), Literal(..), Keyword(..), ControlSequence(..))
import Helpers((?:))
}

%wrapper "posn-strict-text"

-- Basic character categories
$digit = 0-9
$alpha = [a-zA-Z]

-- Whitespace-related tokens
@LineBreak    = "\n" | "\r\n"
@InlineSpace  = [\ \t] | ( \\ @LineBreak)
@BlockComment = "/*" [.\n]* "*/"
@LineComment  = "//" .* ( \\ @LineBreak .*)*

-- Definition of 'Identifier'
$IdentStart = [ $alpha _ ]
$IdentCont  = [ $alpha _ $digit ]
@Identifier = $IdentStart $IdentCont*


-- The complete token set we will lex
tokens :-

    -- All kinds of blanks we ignore
    @InlineSpace+   ;
    @LineComment    ;
    @BlockComment   ;

    -- All control tokens
    ~[\\] ^ @LineBreak  { ctokCtrl LineBreak }
    ";"                 { ctokCtrl Semicolon }
    ":"                 { ctokCtrl Colon }
    ","                 { ctokCtrl Comma }
    "("                 { ctokCtrl OpenParen }
    ")"                 { ctokCtrl CloseParen }
    "{"                 { ctokCtrl OpenBrace }
    "}"                 { ctokCtrl CloseBrace }
    "="                 { ctokCtrl OperAssign}
    "+"                 { ctokCtrl OperAdd }
    "-"                 { ctokCtrl OperSub }
    "*"                 { ctokCtrl OperMul }
    "/"                 { ctokCtrl OperDiv }
    "%"                 { ctokCtrl OperMod }
    "=="                { ctokCtrl OperEquals }
    "!="                { ctokCtrl OperDiffer }
    "!"                 { ctokCtrl OperNot }
    "&&"                { ctokCtrl OperAnd }
    "||"                { ctokCtrl OperOr }
    ">"                 { ctokCtrl OperGt }
    "<"                 { ctokCtrl OperLt }
    ">="                { ctokCtrl OperGe }
    "<="                { ctokCtrl OperLe }

    -- Keywords
    return  { ctokKeyW KeyWReturn }
    if      { ctokKeyW KeyWIf }
    unless  { ctokKeyW KeyWUnless }
    else    { ctokKeyW KeyWElse }
    fun     { ctokKeyW KeyWFun }
    true    { ctokKeyW KeyWTrue }
    false   { ctokKeyW KeyWFalse }
    main    { ctokKeyW KeyWMain }

    -- Literals
    $digit+     { tok (\s -> Literal $ IntLiteral (tRead s)) }

    -- Identifiers
    @Identifier { tok Identifier }
{

-- Each right-hand side has type :: AlexPosn -> Text -> WithPos Token
type Lexer = (AlexPosn -> Text -> WithPos Token)

-- Some action helpers:
tok :: (Text -> Token) -> Lexer
tok f p t = WithPos
  { startPos = mkSourcePos p
  , endPos = mkSourcePos (Text.foldl' alexMove p t)
  , tokenLength = Text.length t
  , tokenVal = f t
  }

ctok :: Token -> Lexer
ctok = tok . const

ctokCtrl :: ControlSequence -> Lexer
ctokCtrl = ctok . Control

ctokKeyW :: Keyword -> Lexer
ctokKeyW = ctok . Keyword

tRead :: Read a => Text -> a
tRead = read . Text.unpack

mkSourcePos :: AlexPosn -> SourcePos
mkSourcePos (AlexPn _ l c) = SourcePos
  { sourceName = ""
  , sourceLine = mkPos l
  , sourceColumn = mkPos c
  }

data WithPos a = WithPos
    { startPos :: SourcePos
    , endPos :: SourcePos
    , tokenLength :: Int
    , tokenVal :: a
    } deriving (Eq, Ord, Show)

-- Patched version of the generated `alexScanTokens`
-- which returns `Left` instead of `error`ing horrendously
myScanTokens :: Text -> Either String [WithPos Token]
myScanTokens str = go (alexStartPos,'\n',[],str)
    where go inp__@(pos,_,_bs,s) =
            case alexScan inp__ 0 of
                AlexEOF -> Right []
                AlexError ((AlexPn _ line column),_,_,_) -> Left $
                    "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp__' _len  -> go inp__'
                AlexToken inp__' len act -> act pos (Data.Text.take len s) ?: go inp__'

}
