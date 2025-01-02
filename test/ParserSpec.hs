{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where
import Test.Hspec (Spec, describe, it, expectationFailure, shouldBe, Expectation)
import Text.Megaparsec (ShowErrorComponent, VisualStream, TraversableStream, ParseErrorBundle, errorBundlePretty)
import Parser2 (pExpression)
import Lexer (showLexError)
import Parser.ParseAndLex (ParseLexError(..), parseAndLex)
import Parser.Shorthands

shouldLexParse :: (Show a, Eq a) => Either ParseLexError a -> a -> Expectation
r `shouldLexParse` v = case r of
    Left e -> case e of
        LexingError le -> expectationFailure $
            "expected: "
                ++ show v
                ++ "\nbut lexing failed with error:\n"
                ++ showLexError le
        ParsingError pe -> expectationFailure $
            "expected: "
                ++ show v
                ++ "\nbut parsing failed with error:\n"
                ++ showBundle pe
    Right x -> x `shouldBe` v

showBundle
    :: (ShowErrorComponent e, VisualStream s, TraversableStream s)
    => ParseErrorBundle s e -> String
showBundle = unlines . fmap indent . lines . errorBundlePretty
  where
    indent x =
      if null x
        then x
        else "  " ++ x


spec :: Spec
spec = do
    describe "parse simple operations" $ do
        it "single number" $
            parseAndLex pExpression "789"
            `shouldLexParse` eaInt 789
        it "parse '1 + 2 * 3' as '1 + (2 *3)" $
            parseAndLex pExpression "1 + 2 * 3"
            `shouldLexParse` eoAdd (eaInt 1) (eoMul (eaInt 2) (eaInt 3))
        it "parse '1 * 2 + 3' as '(1 * 2) + 3" $
            parseAndLex pExpression "1 * 2 + 3"
            `shouldLexParse` eoAdd (eoMul (eaInt 1) (eaInt 2)) (eaInt 3)
