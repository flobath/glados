module Parser.Internal2 (
    Parser,
    pToken,
    pKeyword,
    pControl,
    pSuccess,
    tryConsume,
    tryParse,
    eol,
    pSepBy,
    pCommaSep,
    pBetweenParenthesis,
    pBetweenBrace,
    manyEol,
    pIntLiteral,
    pIdentifier,
) where

import Data.Void (Void)
import Lexer.Tokens (Token (..), Keyword, ControlSequence (..), Literal (IntLiteral))
import Lexer (WithPos(WithPos))
import Text.Megaparsec (Parsec, initialPos, MonadParsec (token), ErrorItem (Tokens), many, between, (<?>), ParseErrorBundle)
import AlexToParsec (TokenStream)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.Set as Set
import Control.Applicative((<|>))
import Data.Functor((<&>), void)
import Data.Maybe (maybeToList)
import Data.Int (Int64)
import Data.Text (Text)

type Parser = Parsec Void TokenStream

pSuccess :: Parser ()
pSuccess = return ()

tryConsume :: Parser a -> Parser ()
tryConsume p = void p <|> return ()

tryParse :: Parser a -> Parser (Maybe a)
tryParse p = (p <&> Just) <|> return Nothing

liftMyToken :: Token -> WithPos Token
liftMyToken = WithPos pos pos 0
  where
    pos = initialPos ""

pToken :: Token -> Parser Token
pToken c = token test (Set.singleton . Tokens . nes . liftMyToken $ c)
  where
    test (WithPos _ _ _ x) =
      if x == c
        then Just x
        else Nothing
    nes x = x :| []

pKeyword :: Keyword -> Parser Token
pKeyword = pToken . Keyword

pControl :: ControlSequence -> Parser Token
pControl = pToken . Control

eol :: Parser Token
eol = pControl LineBreak

manyEol :: Parser [Token]
manyEol = many eol

eos :: Parser Token
eos = pControl LineBreak <|> pControl Semicolon

-- Powerful combinator which parses separators and elements,
-- returning the list of elements.
--
-- It allows both leading and trailing separators, as well as empty lists.
-- The return value of the separator parser is discarded.
pSepBy :: Parser a -> Parser b -> Parser [b]
pSepBy sep p = do
    x <- tryParse p        -- Attempt to parse a leading expr
    xs <- many (sep >> p)  -- Parse all <sep expr>
    tryConsume sep         -- Ignore potential trailing separator

    return (maybeToList x ++ xs)

pCommaSep :: Parser a -> Parser [a]
pCommaSep = pSepBy (pControl Comma *> manyEol)

pBetweenParenthesis :: Parser a -> Parser a
pBetweenParenthesis = between
    (pControl OpenParen >> many eol)
    (pControl CloseParen)

pBetweenBrace :: Parser a -> Parser a
pBetweenBrace = between
    (pControl OpenBrace >> many eol)
    (pControl CloseBrace)

pIntLiteral :: Parser Int64
pIntLiteral = token test Set.empty <?> "integer literal"
    where
        test (WithPos _ _ _ (Literal (IntLiteral n))) = Just n
        test _ = Nothing

pIdentifier :: Parser Text
pIdentifier = token test Set.empty <?> "identifier"
    where
        test (WithPos _ _ _ (Identifier i)) = Just i
        test _ = Nothing
