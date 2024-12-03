{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec.Megaparsec (shouldParse, succeedsLeaving, initialState, shouldFailOn, shouldSucceedOn)
import Test.Hspec (hspec, describe, it)
import Text.Megaparsec (parse, runParser')
import Parser (booleanParser, pDelimiter, pWhiteSpace, integerParser, symbolRefParser)
import Lib (Primitive(Boolean, Constant, SymbolReference), Symbol (Symbol))

main :: IO ()
main = hspec $ do
    describe "pWhitespace" $ do
        it "fail with no whitespace" $
            parse pWhiteSpace "" `shouldFailOn` "not a space"
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
    describe "testFail" $ do
        it "fail 1" $ do
            parse symbolRefParser "" `shouldFailOn` "1234"
        it "fail 2" $ do
            parse symbolRefParser "" `shouldFailOn` "+abc"
        it "fail 3" $ do
            parse symbolRefParser "" `shouldFailOn` "-def"
        it "fail 4" $ do
            parse symbolRefParser "" `shouldFailOn` "-3"
        it "fail 5" $ do
            parse symbolRefParser "" `shouldFailOn` "1234"
        it "fail 6" $ do
            parse symbolRefParser "" `shouldFailOn` "+abc"
        it "fail 7" $ do
            parse symbolRefParser "" `shouldFailOn` "-def"
        it "fail 8" $ do
            parse symbolRefParser "" `shouldFailOn` "-3"
        it "fail 9" $ do
            parse symbolRefParser "" `shouldFailOn` "1234"
        it "fail 10" $ do
            parse symbolRefParser "" `shouldFailOn` "+abc"
