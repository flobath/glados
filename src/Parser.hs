{-# LANGUAGE OverloadedStrings #-}

module Parser (
    booleanParser,
    stringParser,
    integerParser,
    symbolRefParser,
    lambdaParser,
    defineParser,
    expressionParser,
    oppositeParser,
    addParser,
    subtractParser,
    multiplyParser,
    divideParser,
    moduloParser,
    notParser,
    eqParser,
    inferiorParser,
    ifParser,
    callParser,
    parseExpression,
) where

import Parser.Internal

import Text.Megaparsec (
    MonadParsec (try),
    choice,
    (<?>),
    )

import Lib (
    Primitive (Boolean, Constant, SymbolReference, SymbolList),
    Symbol (Symbol),
    Expression (Operation, Primitive),
    ifOperator,
    Arguments (Triple, Single, Pair, List),
    Operator (Unary, Binary, Ternary, Nary),
    defineOperator,
    divideOperator,
    multiplyOperator,
    subtractOperator,
    addOperator,
    oppositeOperator,
    notOperator,
    moduloOperator,
    eqOperator,
    inferiorOperator,
    callOperator,
    lambdaOperator,
    )
import Data.Functor (($>), void, (<&>))
import Control.Applicative ((<|>), Alternative (many, some))
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Lexer (signed)
import Data.Text (Text)

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
symbolRefParser = pLexemeStrict (SymbolReference . Symbol <$> parseSymName)

-- Parser for lambda declaration
lambdaParser :: Parser Expression
lambdaParser = pBetweenParenthesis $ do
    void (pSymbolStrict "lambda")
    paramsLexemes <- pBetweenParenthesis (many (pLexemeStrict parseSymName))
    let params = Primitive $ SymbolList $ map Symbol paramsLexemes
    Operation lambdaOperator . Pair params <$> expressionParser

pSimpleDefine :: Parser Expression
pSimpleDefine = do
    symName <- pLexemeStrict parseSymName
    Operation defineOperator . Pair
        (Primitive (SymbolReference $ Symbol symName))
        <$> expressionParser

pLambdaDefine :: Parser Expression
pLambdaDefine = do
    (funcName:paramNames) <- pBetweenParenthesis $ some $ pLexemeStrict parseSymName
    body <- expressionParser
    let func = Symbol funcName
    let params = Primitive $ SymbolList $ map Symbol paramNames
    return $ Operation defineOperator (Pair
        (Primitive $ SymbolReference func)
        (Operation lambdaOperator (Pair params body)))

defineParser :: Parser Expression
defineParser = pBetweenParenthesis $ do
    void (pSymbolStrict "define")
    pSimpleDefine <|> pLambdaDefine

pSingleExpression :: Parser Arguments
pSingleExpression = Single <$> expressionParser

pExpressionPair :: Parser Arguments
pExpressionPair = Pair <$> expressionParser <*> expressionParser

pExpressionTriple :: Parser Arguments
pExpressionTriple = Triple
    <$> expressionParser
    <*> expressionParser
    <*> expressionParser

parseCall :: Parser Expression
parseCall = pBetweenParenthesis $ do
    exprList <- some parseExpression
    return $ Operation callOperator $ List exprList

pOperation :: Text -> Operator -> Parser Expression
pOperation _ (Nary _) = parseCall
pOperation name op = pBetweenParenthesis $ do
    void (pSymbolStrict name)
    Operation op <$> pArg
    where pArg = case op of
            (Unary _) -> pSingleExpression
            (Binary _) -> pExpressionPair
            (Ternary _) -> pExpressionTriple

oppositeParser,
    addParser,
    subtractParser,
    multiplyParser,
    divideParser,
    moduloParser,
    notParser,
    eqParser,
    inferiorParser,
    ifParser,
    callParser
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
callParser = pOperation "" callOperator

expressionParser :: Parser Expression
expressionParser = Text.Megaparsec.choice (map Text.Megaparsec.try
    [ booleanParser <&> Primitive
    -- , stringParser <&> Primitive
    , integerParser <&> Primitive
    , symbolRefParser <&> Primitive
    , lambdaParser
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
    , callParser
    ]) <?> "an expression"

parseExpression :: Parser Expression
parseExpression = pManyWhiteSpace >> expressionParser
