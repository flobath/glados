{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Megaparsec (Parsec, MonadParsec (lookAhead, hidden, eof, takeWhile1P), choice, skipSome)
import Data.Void (Void)
import Lib (Primitive (Boolean, Constant, SymbolReference), Symbol (Symbol))
import Text.Megaparsec.Char (space1, char)
import Data.Functor (($>), void)
import Control.Applicative ((<|>))
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Lexer (signed)
import Data.Text (Text, unpack)
import Data.Char (isAlphaNum, isSpace, generalCategory, GeneralCategory (UppercaseLetter, LowercaseLetter, TitlecaseLetter, ModifierLetter, OtherLetter, NonSpacingMark, LetterNumber, OtherNumber, DashPunctuation, OtherPunctuation, CurrencySymbol, MathSymbol, ModifierSymbol, OtherSymbol, PrivateUse, DecimalNumber, SpacingCombiningMark, EnclosingMark))

type Parser = Parsec Void Text

-- Helper function to combine predicates
(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||) f1 f2 x = f1 x || f2 x

chezSchemeNonSpaceDelimiter :: String
chezSchemeNonSpaceDelimiter = "()[]#\";"

isChezSchemeDelimiter :: Char -> Bool
isChezSchemeDelimiter = isSpace .|| (`elem` chezSchemeNonSpaceDelimiter)

-- Patched version of Text.MegaParsec.Char.Lexer.space
-- to only succeed if some space is encountered, or at eof
someSpace :: MonadParsec e s m => m a -> m a -> m a -> m ()
someSpace sp line block = skipSome (choice
    [hidden sp, hidden line, hidden block]
    ) <|> eof

-- Parse any amount of whitespace or chez-scheme comments
pWhiteSpace :: Parser ()
pWhiteSpace = someSpace
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
pSymbol :: Text -> Parser Text
pSymbol = L.symbol pDelimiter

-- pSymbol, case insensitive
pSymbol' :: Text -> Parser Text
pSymbol' = L.symbol' pDelimiter

-- Parser for chez-scheme boolean literals #f and #t
booleanParser :: Parser Primitive
booleanParser = (pSymbol' "#t" $> Boolean True) <|> (pSymbol' "#f" $> Boolean False)

-- Parser for chez-scheme string literals
stringParser :: Parser Primitive
stringParser = undefined

-- Parser for chez-scheme integer literals
integerParser :: Parser Primitive
integerParser = Constant <$> pLexeme (signed (return ()) L.decimal)

-- Parser for symbol references
symbolRefParser :: Parser Primitive
symbolRefParser = pLexeme (SymbolReference . Symbol . unpack <$> parseSymName)
    where parseSymName = takeWhile1P
            (Just "viable symbol character")
            (not . isChezSchemeDelimiter)

-- Parser for lambda declaration
lambdaParser :: Parser Primitive
lambdaParser = undefined
