{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where
import Test.Hspec (Spec, describe, it, expectationFailure, shouldBe, Expectation, HasCallStack)
import Text.Megaparsec (ShowErrorComponent, VisualStream, TraversableStream, ParseErrorBundle, errorBundlePretty)
import Parser2 (pExpression)
import Lexer (showLexError)
import Parser.ParseAndLex (ParseLexError(..), parseAndLex)
import Parser.Shorthands
import Test.Hspec (Spec, describe, it, shouldBe)
import Parser2 (pExpression, pTypeIdentifier, pVariableDecl)
import Parser.ParseAndLex (parseAndLex)
import Parser.Shorthands

shouldLexParse :: (HasCallStack, Show a, Eq a) => Either ParseLexError a -> a -> Expectation
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
    describe "parse simple expressions" $ do
        it "single number" $
            parseAndLex pExpression "789"
            `shouldLexParse` eaInt 789
        it "single identifier" $
            parseAndLex pExpression "myidentifier"
            `shouldLexParse` eaId "myidentifier"
        it "single false literal" $
            parseAndLex pExpression "false"
            `shouldLexParse` eaBool False
        it "single true literal" $
            parseAndLex pExpression "true"
            `shouldLexParse` eaBool True
        it "parse '1 + 2 * 3' as '1 + (2 * 3)" $
            parseAndLex pExpression "1 + 2 * 3"
            `shouldLexParse` eoAdd (eaInt 1) (eoMul (eaInt 2) (eaInt 3))
        it "parse '1 * 2 + 3' as '(1 * 2) + 3" $
            parseAndLex pExpression "1 * 2 + 3"
            `shouldLexParse` eoAdd (eoMul (eaInt 1) (eaInt 2)) (eaInt 3)
        it "parenthesised expressions" $
            parseAndLex pExpression "(1 + 9) / abc"
            `shouldLexParse` eoDiv (eoAdd (eaInt 1) (eaInt 9)) (eaId "abc")

        it "parse conditional expression" $
            parseAndLex pExpression "if (myvar > 8 * 4) 8 + 3 else falsecondition"
            `shouldLexParse` eIf
                (eoGt (eaId "myvar") (eoMul (eaInt 8) (eaInt 4)))
                (eoAdd (eaInt 8) (eaInt 3))
                (Just (eaId "falsecondition"))

    describe "variable declarations" $ do
        it "type declaration" $
            parseAndLex pTypeIdentifier "i32"
            `shouldLexParse` tId "i32"
        it "full variable declaration" $
            parseAndLex pVariableDecl "bool myvar"
            `shouldLexParse` vdecl (tId "bool") (vId "myvar")

    describe "function calls" $ do
        it "call with no arguments" $
            parseAndLex pExpression "myfunction()"
            `shouldLexParse` eCall (eaId "myfunction") []
        it "call with 1 arg, no trailing comma" $
            parseAndLex pExpression "abc(false)"
            `shouldLexParse` eCall (eaId "abc") [eaBool False]
        it "call with 1 arg, with trailing comma" $
            parseAndLex pExpression "abc(hello,)"
            `shouldLexParse` eCall (eaId "abc") [eaId "hello"]
        it "call with 3 args, no trailing comma" $
            parseAndLex pExpression "myfunc(132, abc, 75)"
            `shouldLexParse` eCall (eaId "myfunc") [eaInt 132, eaId "abc", eaInt 75]
        it "call with 3 args, trailing comma and nested call" $
            parseAndLex pExpression "myfunc(79, otherfunc(somevar), true,)"
            `shouldLexParse` eCall (eaId "myfunc")
                [ eaInt 79
                , eCall (eaId "otherfunc") [eaId "somevar"]
                , eaBool True
                ]
        it "multiline call with multiple arguments" $
            parseAndLex pExpression "f(\n123,\n42,\n\n33,\n)"
            `shouldLexParse` eCall (eaId "f") [eaInt 123, eaInt 42, eaInt 33]
