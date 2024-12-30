module Parser2 (
) where

import qualified Data.Set as Set
import Text.Megaparsec (
    (<?>),
    token, choice,
    )

import Data.Functor((<&>), ($>))
import Control.Applicative((<|>))

import Parser.Internal2

import Lexer.Tokens (Literal(..), Token(..), Keyword (..))
import Lexer (WithPos(WithPos))
import Data.Int (Int64)
import Data.Text (Text)
import Parser.AST (
    VarIdentifier (VarIdentifier),
    TypeIdentifier (TypeIdentifier),
    AtomicExpression (..),
    )

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

pVarIdentifier :: Parser VarIdentifier
pVarIdentifier = pIdentifier <&> VarIdentifier <?> "variable identifier"

pTypeIdentifier :: Parser TypeIdentifier
pTypeIdentifier = pIdentifier <&> TypeIdentifier <?> "type identifier"

pBoolean :: Parser Bool
pBoolean = (pKeyword KeyWTrue $> True
        <|> pKeyword KeyWFalse $> False)
        <?> "boolean literal"

pAtom :: Parser AtomicExpression
pAtom = choice
    [ pIntLiteral       <&> AtomIntLiteral
    , pVarIdentifier    <&> AtomIdentifier
    , pBoolean          <&> AtomBooleanLiteral
    ]
