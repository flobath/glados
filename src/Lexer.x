{
module Lexer (Token(..), AlexPosn(..), alexScanTokens) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Megaparsec(SourcePos(..), mkPos)
}

%wrapper "posn-strict-text"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  let					{ tok (const Let) }
  in					{ tok (const In) }
  $digit+				{ tok (\s -> Int (tRead s)) }
  [\=\+\-\*\/\(\)]			{ tok (\s -> Sym (Text.head s)) }
  $alpha [$alpha $digit \_ \']*		{ tok Var }

{
-- Each right-hand side has type :: AlexPosn -> Text -> Token

-- Some action helpers:
tok :: (Text -> Token) -> AlexPosn -> Text -> WithPos Token
tok f p t = WithPos
  { startPos = mkSourcePos p
  , endPos = mkSourcePos (Text.foldl' alexMove p t)
  , tokenLength = Text.length t
  , tokenVal = f t
  }

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

-- The token type:
data Token
    = Let
    | In
    | Sym Char
    | Var Text
    | Int Int
    deriving (Eq,Show)
}
