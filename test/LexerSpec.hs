{-# LANGUAGE OverloadedStrings #-}

module LexerSpec (spec) where
import Test.Hspec (describe, shouldBe, it, Spec, SpecWith)
import Lexer (myScanTok, Token (Control, Keyword, Identifier, Literal))
import Lexer.Tokens (ControlSequence(..), Keyword (..), Literal (IntLiteral))
import Data.Text (Text)

expectSingleToken :: String -> Text -> Token -> SpecWith ()
expectSingleToken name input expected = it name $
    myScanTok input `shouldBe` Right [expected]

spec :: Spec
spec = do
    describe "Lex a linebreak" $ do
        it "single \\n" $
            myScanTok "\n" `shouldBe` Right [Control LineBreak]
        it "single \\r\\n" $
            myScanTok "\r\n" `shouldBe` Right [Control LineBreak]
        it "escaped \\n" $
            myScanTok "\\\n" `shouldBe` Right []
        it "escaped \\r\\n" $
            myScanTok "\\\r\n" `shouldBe` Right []

    describe "Lex some identifiers" $ do
        expectSingleToken "'abc' identifier" "abc" $ Identifier "abc"
        expectSingleToken "'MyVar1234' identifier" "MyVar1234" $ Identifier "MyVar1234"

    describe "Lex some numbers" $ do
        expectSingleToken "123" "123" $ Literal $ IntLiteral 123
        expectSingleToken "45645622" "45645622" $ Literal $ IntLiteral 45645622

    describe "Lex all control sequences" $ do
        expectSingleToken "semicolon" ";" $ Control Semicolon
        expectSingleToken "colon" ":" $ Control Colon
        expectSingleToken "comma" "," $ Control Comma
        expectSingleToken "open paren" "(" $ Control OpenParen
        expectSingleToken "close paren" ")" $ Control CloseParen
        expectSingleToken "open brace" "{" $ Control OpenBrace
        expectSingleToken "close brace" "}" $ Control CloseBrace
        expectSingleToken "assign operator" "=" $ Control OperAssign
        expectSingleToken "add operator" "+" $ Control OperAdd
        expectSingleToken "sub operator" "-" $ Control OperSub
        expectSingleToken "mul operator" "*" $ Control OperMul
        expectSingleToken "div operator" "/" $ Control OperDiv
        expectSingleToken "mod operator" "%" $ Control OperMod
        expectSingleToken "equals operator" "==" $ Control OperEquals
        expectSingleToken "differs operators" "!=" $ Control OperDiffer
        expectSingleToken "not operator" "!" $ Control OperNot
        expectSingleToken "and operator" "&&" $ Control OperAnd
        expectSingleToken "or operator" "||" $ Control OperOr
        expectSingleToken "greater than operator" ">" $ Control OperGt
        expectSingleToken "greater than or equals operator" ">=" $ Control OperGe
        expectSingleToken "lesser than operator" "<" $ Control OperLt
        expectSingleToken "lesser than or equals operator" "<=" $ Control OperLe

    describe "Lex all keywords" $ do
        expectSingleToken "return keyword" "return" $ Keyword KeyWReturn
        expectSingleToken "if keyword" "if" $ Keyword KeyWIf
        expectSingleToken "unless keyword" "unless" $ Keyword KeyWUnless
        expectSingleToken "else keyword" "else" $ Keyword KeyWElse
        expectSingleToken "fun keyword" "fun" $ Keyword KeyWFun
        expectSingleToken "true keyword" "true" $ Keyword KeyWTrue
        expectSingleToken "false keyword" "false" $ Keyword KeyWFalse
        expectSingleToken "main keyword" "main" $ Keyword KeyWMain

    describe "Lex a full function definition" $ do
        it "sum function" $ do
            myScanTok
                "fun my_add(i32 a, i32 b): i32\n\
                \{\n\
                \    i32 result = a + b\n\
                \\n\
                \    return result\n\
                \}\n"
            `shouldBe` Right
                [ Keyword KeyWFun
                , Identifier "my_add"
                , Control OpenParen
                , Identifier "i32"
                , Identifier "a"
                , Control Comma
                , Identifier "i32"
                , Identifier "b"
                , Control CloseParen
                , Control Colon
                , Identifier "i32"
                , Control LineBreak
                , Control OpenBrace
                , Control LineBreak
                , Identifier "i32"
                , Identifier "result"
                , Control OperAssign
                , Identifier "a"
                , Control OperAdd
                , Identifier "b"
                , Control LineBreak
                , Control LineBreak
                , Keyword KeyWReturn
                , Identifier "result"
                , Control LineBreak
                , Control CloseBrace
                , Control LineBreak
                ]
