{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Parser where

import Text.Megaparsec
    ( Parsec
    , MonadParsec (lookAhead, hidden, eof, takeWhile1P, try)
    , choice
    , skipSome
    , between
    )
import Data.Void (Void)
import Lib
    ( Primitive (Boolean, Constant, SymbolReference, Function, SymbolList)
    , Symbol (Symbol)
    , Expression (Operation, Primitive)
    , ifOperator
    , Arguments (Triple, Single, Pair, List)
    , Operator (Unary, Binary, Ternary, Nary)
    , defineOperator
    , divideOperator
    , multiplyOperator
    , subtractOperator
    , addOperator
    , oppositeOperator
    , notOperator
    , moduloOperator
    , eqOperator
    , inferiorOperator
    )
import Text.Megaparsec.Char (space1, char)
import Data.Functor (($>), void, (<&>))
import Control.Applicative ((<|>), Alternative (many))
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Lexer (signed)
import Data.Text (Text, unpack, uncons, all, splitAt)
import Data.Char
    ( isSpace
    , generalCategory
    , GeneralCategory
        ( UppercaseLetter
        , LowercaseLetter
        , TitlecaseLetter
        , ModifierLetter
        , OtherLetter
        , NonSpacingMark
        , LetterNumber
        , OtherNumber
        , DashPunctuation
        , OtherPunctuation
        , CurrencySymbol
        , MathSymbol
        , ModifierSymbol
        , OtherSymbol
        , PrivateUse
        , DecimalNumber
        , SpacingCombiningMark
        , EnclosingMark
        )
    )

type Parser = Parsec Void Text

-- Helper function to combine predicates
(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||) f1 f2 x = f1 x || f2 x

(<<|>>) :: Alternative f => (t -> f a) -> (t -> f a) -> t -> f a
(<<|>>) p1 p2 p = p1 p <|> p2 p

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
isChezSchemeDelimiter = isSpace .|| (`elem` chezSchemeNonSpaceDelimiter)

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

-- Parser for chez-scheme boolean literals #f and #t
booleanParser :: Parser Primitive
booleanParser = (pSymbolStrict' "#t" $> Boolean True) <|> (pSymbolStrict' "#f" $> Boolean False)

-- Parser for chez-scheme string literals
stringParser :: Parser Primitive
stringParser = undefined

-- Parser for chez-scheme integer literals
integerParser :: Parser Primitive
integerParser = Constant <$> pLexemeStrict (signed (return ()) L.decimal)

-- Parser for symbol references
symbolRefParser :: Parser Primitive
symbolRefParser = pLexemeStrict (SymbolReference . Symbol . unpack <$> parseSymName)

-- Parser for lambda declaration
lambdaParser :: Parser Primitive
lambdaParser = pBetweenParenthesis $ do
    void (pSymbolStrict "lambda")
    params <- pBetweenParenthesis (many (pLexemeStrict parseSymName))
    Function (map (Symbol . unpack) params) <$> expressionParser

defineParser :: Parser Expression
defineParser = pBetweenParenthesis $ do
    void (pSymbolStrict "define")
    symName <- pLexemeStrict parseSymName
    Operation defineOperator . Pair
        (Primitive (SymbolList [Symbol (unpack symName)]))
        <$> expressionParser

oppositeParser,
    addParser,
    subtractParser,
    multiplyParser,
    divideParser,
    moduloParser,
    notParser,
    eqParser,
    inferiorParser,
    ifParser
    :: Parser Expression
oppositeParser = pOperation "-" oppositeOperator
addParser = pOperation "+" addOperator
subtractParser = pOperation "-" subtractOperator
multiplyParser = pOperation "*" multiplyOperator
divideParser = pOperation "div" divideOperator
moduloParser = pOperation "mod" moduloOperator
notParser = pOperation "not" notOperator
eqParser = pOperation "eq?" eqOperator
inferiorParser = pOperation "<" inferiorOperator
ifParser = pOperation "if" ifOperator

expressionParser :: Parser Expression
expressionParser = choice
    [ booleanParser <&> Primitive
    -- , stringParser <&> Primitive
    , integerParser <&> Primitive
    , symbolRefParser <&> Primitive
    , lambdaParser <&> Primitive
    , defineParser
    , oppositeParser
    , addParser
    , subtractParser
    , multiplyParser
    , divideParser
    , moduloParser
    , notParser
    , eqParser
    , inferiorParser
    , ifParser
    ]

pSingleExpression :: Parser Arguments
pSingleExpression = Single <$> expressionParser

pExpressionPair :: Parser Arguments
pExpressionPair = Pair <$> expressionParser <*> expressionParser

pExpressionTriple :: Parser Arguments
pExpressionTriple = Triple
    <$> expressionParser
    <*> expressionParser
    <*> expressionParser

pOperation :: Text -> Operator -> Parser Expression
pOperation name op = pBetweenParenthesis $ do
    void (pSymbolStrict name)
    Operation op <$> pArg
    where pArg = case op of
            (Unary _) -> pSingleExpression
            (Binary _) -> pExpressionPair
            (Ternary _) -> pExpressionTriple
