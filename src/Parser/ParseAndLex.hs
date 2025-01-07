module Parser.ParseAndLex (
    ParseLexError(..),
    parseAndLex,
) where

import Parser.Internal2 (ParserError, Parser)
import Lexer (LexerError, myScanTokens, showLexError)
import Data.Text (Text)
import Data.Bifunctor (Bifunctor(first))
import Text.Megaparsec (runParser, errorBundlePretty)
import AlexToParsec (TokenStream(TokenStream, myStreamInput, unTokenStream))

-- Helper which maps over the error variant of an 'Either'
(>&<) :: Bifunctor p => p a c -> (a -> b) -> p b c
(>&<) = flip first

data ParseLexError = LexingError LexerError | ParsingError ParserError

instance Show ParseLexError where
    show (LexingError err) = showLexError err
    show (ParsingError err) = errorBundlePretty err

parseAndLex :: Parser a -> Text -> Either ParseLexError a
parseAndLex parser input = do
    tokens <- myScanTokens input >&< LexingError
    let tokStream = TokenStream{ myStreamInput = input, unTokenStream = tokens}

    runParser parser "" tokStream >&< ParsingError
