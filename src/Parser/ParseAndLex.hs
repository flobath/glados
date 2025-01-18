module Parser.ParseAndLex (
    ParseLexError(..),
    parseAndLexFile,
    parseAndLex,
) where

import Parser.Internal (ParserError, Parser)
import Lexer (LexerError, myScanTokens, showLexError)
import Data.Text (Text)
import Text.Megaparsec (runParser, errorBundlePretty)
import AlexToParsec (TokenStream(TokenStream, myStreamInput, unTokenStream))
import Helpers((>&<))

data ParseLexError = LexingError LexerError | ParsingError ParserError

instance Show ParseLexError where
    show (LexingError err) = showLexError err
    show (ParsingError err) = "parse error:\n" ++ errorBundlePretty err

parseAndLexFile :: FilePath -> Parser a -> Text -> Either ParseLexError a
parseAndLexFile file parser input = do
    tokens <- myScanTokens file input >&< LexingError
    let tokStream = TokenStream{ myStreamInput = input, unTokenStream = tokens}

    runParser parser file tokStream >&< ParsingError

parseAndLex :: Parser a -> Text -> Either ParseLexError a
parseAndLex = parseAndLexFile ""
