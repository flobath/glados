module Parser where

import Text.Megaparsec (Parsec, MonadParsec (lookAhead), choice)
import Data.Void (Void)
import Lib (Primitive (Boolean))
import Text.Megaparsec.Char (space1, char)
import Data.Functor (($>), void)
import Control.Applicative ((<|>))
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Parse any amount of whitespace or chez-scheme comments
pWhiteSpace :: Parser ()
pWhiteSpace = L.space
    space1
    (L.skipLineComment ";")
    (L.skipBlockComment "#|" "|#")

-- Check if the next character is part of
-- the string passed in argument, but do not consume it
pDelimiterCharacter :: String -> Parser ()
pDelimiterCharacter = choice . map (void . lookAhead . char)

-- Parse delimiter between two identifiers:
-- - either some whitespace/comment (which will be consumed)
-- - or the next character is a delimiter which will not be consumed
pDelimiter :: Parser ()
pDelimiter = pWhiteSpace <|> pDelimiterCharacter "()[]#\""

-- Parse a chez-scheme lexeme: apply the parser
-- passed as parameter and then check make sure we
-- reached a delimiter, and consume it appropriately
pLexeme :: Parser a -> Parser a
pLexeme = L.lexeme pDelimiter

-- Similar to pLexeme but intended for parsing know strings
pSymbol :: String -> Parser String
pSymbol = L.symbol pDelimiter

-- Parser for chez-scheme boolean literals #f and #t
booleanParser :: Parser Primitive
booleanParser = (pSymbol "#t" $> Boolean True) <|> (pSymbol "#f" $> Boolean False)
