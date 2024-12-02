{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec.Megaparsec (shouldParse, succeedsLeaving, initialState, shouldFailOn)
import Test.Hspec (hspec, describe, it)
import Text.Megaparsec (parse, runParser')
import Parser (booleanParser, pDelimiter, pWhiteSpace, integerParser)
import Lib (Primitive(Boolean, Constant))

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
