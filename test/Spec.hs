{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec.Megaparsec (shouldParse, succeedsLeaving, initialState, shouldFailOn, shouldSucceedOn)
import Test.Hspec (hspec, describe, it)
import Text.Megaparsec (parse, runParser')
import Parser
    ( booleanParser
    , pDelimiter
    , pSomeWhiteSpace
    , integerParser
    , symbolRefParser
    , addParser
    , expressionParser
    , defineParser
    )
import Lib
    ( Primitive(Boolean, Constant, SymbolReference, SymbolList)
    , Symbol (Symbol)
    , Expression (Primitive, Operation)
    , addOperator
    , Arguments (Pair, List)
    , defineOperator
    )

main :: IO ()
main = hspec $ do
    describe "pSomeWhitespace" $ do
        it "fail with no whitespace" $
            parse pSomeWhiteSpace "" `shouldFailOn` "not a space"
    describe "pDelimiter" $ do
        it "parse any whitespace" $
            runParser' pDelimiter (initialState "   \n\n\t  \t") `succeedsLeaving` ""
        it "fail in middle of a word" $
            parse pDelimiter "" `shouldFailOn` "abc"

    describe "parseBoolean" $ do
        it "parse a simple #t" $
            parse booleanParser "" "#t" `shouldParse` Boolean True
        it "parse a simple #f" $
            parse booleanParser "" "#f" `shouldParse` Boolean False

        it "parse an uppercase #t" $
            parse booleanParser "" "#T" `shouldParse` Boolean True
        it "parse an uppercase #f" $
            parse booleanParser "" "#F" `shouldParse` Boolean False

        it "consume trailing whitespace" $
            runParser' booleanParser (initialState "#t  \n   \t(abc") `succeedsLeaving` "(abc"

        it "fail with #fabcd" $
            parse booleanParser "" `shouldFailOn` "#fabcd"

        it "stops at opening parenthesis" $
            runParser' booleanParser (initialState "#f(hi)") `succeedsLeaving` "(hi)"
        it "stops at opening quote" $
            runParser' booleanParser (initialState "#f\"hi\"") `succeedsLeaving` "\"hi\""
        it "stops at following boolean literal" $
            runParser' booleanParser (initialState "#f#t") `succeedsLeaving` "#t"

    describe "parseInteger" $ do
        it "parse number 42" $ do
            parse integerParser "" "42 some other string content" `shouldParse` Constant 42
        it "parse number -175" $ do
            parse integerParser "" "-175" `shouldParse` Constant (-175)

    describe "parseSymbolRef" $ do
        it "parse a simple 'abc' symbol" $ do
            parse symbolRefParser "" "abc" `shouldParse` SymbolReference (Symbol "abc")

        it "parse '+' in an addition" $ do
            let ret@(_, res) = runParser' symbolRefParser (initialState "+\t4 8)")
            ret `succeedsLeaving` "4 8)"
            res `shouldParse` SymbolReference (Symbol "+")

        it "stop at opening parenthesis" $ do
            let ret@(_, res) = runParser' symbolRefParser (initialState "my!super$@symb0ln4me()")
            ret `succeedsLeaving` "()"
            res `shouldParse` SymbolReference (Symbol "my!super$@symb0ln4me")

        it "fail on number" $ do
            parse symbolRefParser "" `shouldFailOn` "1234"
        it "fail on +abc" $ do
            parse symbolRefParser "" `shouldFailOn` "+abc"
        it "fail on -def" $ do
            parse symbolRefParser "" `shouldFailOn` "-def"
        it "fail on -3" $ do
            parse symbolRefParser "" `shouldFailOn` "-3"

        it "succeed on +" $ do
            parse symbolRefParser "" `shouldSucceedOn` "+"
        it "succeed on -" $ do
            parse symbolRefParser "" `shouldSucceedOn` "+"

    describe "addition" $ do
        it "parse a simple addition" $ do
            parse addParser "" "(+ 4 8)" `shouldParse` Operation addOperator (Pair (Primitive $ Constant 4) (Primitive $ Constant 8))

    describe "parse (define..." $ do
        it "define a variable to a number" $ do
            parse defineParser "" "(define abc 8)" `shouldParse` Operation defineOperator (Pair (Primitive $ SymbolList [Symbol "abc"]) (Primitive (Constant 8)))
    describe "functional tests" $ do
        it "parse a factorial function" $ do
            parse expressionParser ""
            `shouldSucceedOn` "(define fact (lambda (x) (if (eq? x 1) 1 (* x (fact (- x 1))))))"
