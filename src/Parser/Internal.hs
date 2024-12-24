{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Parser.Internal (
    Parser,
    chezSchemeNonSpaceDelimiter,
    isChezSchemeSymbolInitial,
    isChezSchemeSymbolSubsequent,
    isChezSchemeDelimiter,
    someSpace,
    pSomeWhiteSpace,
    pManyWhiteSpace,
    pDelimiterCharacter,
    pDelimiter,
    pLexemeStrict,
    pLexeme,
    pSymbolStrict,
    pSymbolStrict',
    pSymbol,
    parseUntilDelimiter,
    parseSymName,
    pBetweenParenthesis,
) where

import Text.Megaparsec (
    Parsec,
    MonadParsec (lookAhead, hidden, eof, takeWhile1P),
    choice,
    skipSome,
    between,
    )

import Data.Void (Void)
import Text.Megaparsec.Char (space1, char)
import Data.Functor (void)
import Control.Applicative ((<|>), Alternative)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, uncons, all, splitAt)
import Data.Char (isSpace, generalCategory, GeneralCategory (..))

type Parser = Parsec Void Text

comb :: (b -> b -> b) -> (a -> b) -> (a -> b) -> (a -> b)
comb combinator f1 f2 input = f1 input `combinator` f2 input

-- Helper function to combine predicates
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = comb (||)

(<<|>>) :: Alternative f => (t -> f a) -> (t -> f a) -> t -> f a
(<<|>>) = comb (<|>)

chezSchemeNonSpaceDelimiter :: String
chezSchemeNonSpaceDelimiter = "()[]#\";"

-- Checks if a character is a valid initial for a symbol name,
-- as defined in https://scheme.com/tspl4/grammar.html#APPENDIXFORMALSYNTAX
isChezSchemeSymbolInitial :: Char -> Bool
isChezSchemeSymbolInitial c
    = c `elem` basicLetters
    || (c > '\x7f' && generalCategory c `elem` [
        UppercaseLetter,
        LowercaseLetter,
        TitlecaseLetter,
        ModifierLetter,
        OtherLetter,
        NonSpacingMark,
        LetterNumber,
        OtherNumber,
        DashPunctuation,
        OtherPunctuation,
        CurrencySymbol,
        MathSymbol,
        ModifierSymbol,
        OtherSymbol,
        PrivateUse
    ])
    where basicLetters = ['a'..'z'] ++ ['A'..'Z'] ++ "!$%&*/:<=>?~_^"

-- Checks if a character is a valid non-initial for a symbol name,
-- as defined in https://scheme.com/tspl4/grammar.html#APPENDIXFORMALSYNTAX
isChezSchemeSymbolSubsequent :: Char -> Bool
isChezSchemeSymbolSubsequent c
    = isChezSchemeSymbolInitial c
    || c `elem` ['0'..'9'] ++ ".+-@"
    || generalCategory c `elem` [
        DecimalNumber,
        SpacingCombiningMark,
        EnclosingMark
    ]

isChezSchemeDelimiter :: Char -> Bool
isChezSchemeDelimiter = isSpace <||> (`elem` chezSchemeNonSpaceDelimiter)

-- Patched version of Text.MegaParsec.Char.Lexer.space
-- to only succeed if some space is encountered, or at eof
someSpace :: MonadParsec e s m => m a -> m a -> m a -> m ()
someSpace sp line block = skipSome (choice
    [hidden sp, hidden line, hidden block]
    ) <|> eof

-- Parse any amount of whitespace or chez-scheme comments
pSomeWhiteSpace :: Parser ()
pSomeWhiteSpace = someSpace
    space1
    (L.skipLineComment ";")
    (L.skipBlockComment "#|" "|#")

-- Parse any amount of whitespace or chez-scheme comments
pManyWhiteSpace :: Parser ()
pManyWhiteSpace = L.space
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
pDelimiter = pSomeWhiteSpace <|> pDelimiterCharacter chezSchemeNonSpaceDelimiter

-- Parse a chez-scheme lexeme: apply the parser
-- passed as parameter and then check make sure we
-- reached a delimiter, and consume it appropriately
pLexemeStrict :: Parser a -> Parser a
pLexemeStrict = L.lexeme pDelimiter

-- Apply a given parser and consume any following
-- chez-scheme whitespace or comments, if some is found
pLexeme :: Parser a -> Parser a
pLexeme = L.lexeme pManyWhiteSpace

-- Similar to pLexemeStrict but intended for parsing know strings
pSymbolStrict :: Text -> Parser Text
pSymbolStrict = L.symbol pDelimiter

-- pSymbolStrict, case insensitive
pSymbolStrict' :: Text -> Parser Text
pSymbolStrict' = L.symbol' pDelimiter

-- Similar to pLexeme but intended for parsing know strings
pSymbol :: Text -> Parser Text
pSymbol = L.symbol pManyWhiteSpace

parseUntilDelimiter :: Parser Text
parseUntilDelimiter = takeWhile1P
    (Just "any non-delimiter character")
    (not . isChezSchemeDelimiter)

parseSymName :: Parser Text
parseSymName = do
    tok <- parseUntilDelimiter
    if checkSymName tok then
        return tok
    else
        fail "Symbol contains invalid character(s)"

    where
        -- Symbol names can contain almost any characters but
        -- have very weird constraints.
        -- See https://scheme.com/tspl4/grammar.html#Strings for details
        checkSymName :: Text -> Bool
        checkSymName "+" = True
        checkSymName "-" = True
        checkSymName "..." = True
        checkSymName (Data.Text.splitAt 2 -> ("->", subsequents))
            = Data.Text.all isChezSchemeSymbolSubsequent subsequents
        checkSymName (uncons -> (Just (initial, subsequents)))
            = isChezSchemeSymbolInitial initial
            && Data.Text.all isChezSchemeSymbolSubsequent subsequents
        checkSymName _ = False

pBetweenParenthesis :: Parser a -> Parser a
pBetweenParenthesis
    = between (pSymbol "(") (pSymbol ")")
    <<|>> between (pSymbol "[") (pSymbol "]")
