{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where
import Test.Hspec (
    Spec,
    describe,
    it,
    expectationFailure,
    shouldBe,
    Expectation,
    HasCallStack,
    Spec,
    describe,
    it,
    shouldBe,
    context,
    )
import Text.Megaparsec (ShowErrorComponent, VisualStream, TraversableStream, ParseErrorBundle, errorBundlePretty, parse, Parsec)
import Parser (
    pExpression,
    pExpression,
    pTypeIdentifier,
    pVariableDecl,
    pStatement,
    pBlockExpression,
    pFunction,
    pMainFunction,
    pProgram,
    )
import Parser.WithPos(withPos)
import Lexer (showLexError, alexScanTokens)
import Lexer.Tokens (
    Token(..),
    ControlSequence(..),
    )
import Parser.ParseAndLex (
    ParseLexError(..),
    parseAndLex,
    parseAndLex
    )
import Parser.Shorthands
import Parser.AST (BlockExpression(BlockExpression), Function (Function), MainFunction (MainFunction), Program (Program))
import Test.Hspec.Megaparsec (etok, err, utok, shouldFailWith, elabel)
import Parser.Internal (liftMyToken)
import AlexToParsec (TokenStream(..))
import Data.Text (Text)

-- Helper function to combine the alex lexer with a parser
lexParse :: Parsec e TokenStream a -> Text -> Either (ParseErrorBundle TokenStream e) a
lexParse p input = parse p "" TokenStream
    { myStreamInput = input
    , unTokenStream = alexScanTokens input
    }

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
    describe "atomic expressions" $ do
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

    describe "all operations" $ do
        it "negation (-)" $
            parseAndLex pExpression "-a"
            `shouldLexParse` eoNeg (eaId "a")
        it "plus" $
            parseAndLex pExpression "+a"
            `shouldLexParse` eoPlus (eaId "a")
        it "negation (!)" $
            parseAndLex pExpression "!a"
            `shouldLexParse` eoNot (eaId "a")
        it "addition" $
            parseAndLex pExpression "a + b"
            `shouldLexParse` eoAdd (eaId "a") (eaId "b")
        it "subtraction" $
            parseAndLex pExpression "a - b"
            `shouldLexParse` eoSub (eaId "a") (eaId "b")
        it "multiplication" $
            parseAndLex pExpression "a * b"
            `shouldLexParse` eoMul (eaId "a") (eaId "b")
        it "division" $
            parseAndLex pExpression "a / b"
            `shouldLexParse` eoDiv (eaId "a") (eaId "b")
        it "modulo" $
            parseAndLex pExpression "a % b"
            `shouldLexParse` eoMod (eaId "a") (eaId "b")
        it "greater than" $
            parseAndLex pExpression "a > b"
            `shouldLexParse` eoGt (eaId "a") (eaId "b")
        it "lesser than" $
            parseAndLex pExpression "a < b"
            `shouldLexParse` eoLt (eaId "a") (eaId "b")
        it "greater than or equals" $
            parseAndLex pExpression "a >= b"
            `shouldLexParse` eoGe (eaId "a") (eaId "b")
        it "lesser than or equals" $
            parseAndLex pExpression "a <= b"
            `shouldLexParse` eoLe (eaId "a") (eaId "b")
        it "equals" $
            parseAndLex pExpression "a == b"
            `shouldLexParse` eoEq (eaId "a") (eaId "b")
        it "not equals" $
            parseAndLex pExpression "a != b"
            `shouldLexParse` eoNeq (eaId "a") (eaId "b")
        it "and" $
            parseAndLex pExpression "a && b"
            `shouldLexParse` eoAnd (eaId "a") (eaId "b")
        it "or" $
            parseAndLex pExpression "a || b"
            `shouldLexParse` eoOr (eaId "a") (eaId "b")

    describe "operator priority" $ do
        it "parse '1 + 2 * 3' as '1 + (2 * 3)" $
            parseAndLex pExpression "1 + 2 * 3"
            `shouldLexParse` eoAdd (eaInt 1) (eoMul (eaInt 2) (eaInt 3))
        it "parse '1 * 2 + 3' as '(1 * 2) + 3" $
            parseAndLex pExpression "1 * 2 + 3"
            `shouldLexParse` eoAdd (eoMul (eaInt 1) (eaInt 2)) (eaInt 3)
        it "parenthesised expressions" $
            parseAndLex pExpression "(1 + 9) / abc"
            `shouldLexParse` eoDiv (eoAdd (eaInt 1) (eaInt 9)) (eaId "abc")

    describe "conditionals" $ do
        it "if conditional without else arm" $
            parseAndLex pExpression "if (a) b"
            `shouldLexParse` eIf (eaId "a") (eaId "b") Nothing
        it "if conditional with an else arm" $
            parseAndLex pExpression "if (myvar > 8 * 4) 8 + 3 else falsecondition"
            `shouldLexParse` eIf
                (eoGt (eaId "myvar") (eoMul (eaInt 8) (eaInt 4)))
                (eoAdd (eaInt 8) (eaInt 3))
                (Just (eaId "falsecondition"))
        it "unless conditional" $
            parseAndLex pExpression "unless (a) b"
            `shouldLexParse` eIf (eoNot $ eaId "a") (eaId "b") Nothing

    describe "loops" $ do
        it "while loop" $
            parseAndLex pExpression "while (myvar > 8 * 4) 8 + 3"
            `shouldLexParse` eWhile
                (eoGt (eaId "myvar") (eoMul (eaInt 8) (eaInt 4)))
                (eoAdd (eaInt 8) (eaInt 3))

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

    describe "variable declarations" $ do
        it "type declaration" $
            parseAndLex pTypeIdentifier "i32"
            `shouldLexParse` tId "i32"
        it "full variable declaration" $
            parseAndLex pVariableDecl "bool myvar"
            `shouldLexParse` vdecl "bool" "myvar"

    describe "basic statements" $ do
        it "return statement" $
            parseAndLex pStatement "return a;"
            `shouldLexParse` sRet (eaId "a")
        it "var decl statement (no value)" $
            parseAndLex pStatement "i32 myint;"
            `shouldLexParse` sDecl (tId "i32") (vId "myint") Nothing
        it "var decl statement with initialiser" $
            parseAndLex pStatement "i32 myint = 42\n"
            `shouldLexParse` sDecl
                (tId "i32")
                (vId "myint")
                (Just $ eaInt 42)
        context "assignment statement" $ do
            it "basic succes" $
                parseAndLex pStatement "abc = 4;"
                `shouldLexParse` sAssi "abc" (eaInt 4)
            it "assign to function call" $
                parseAndLex pStatement "x = f(a, b)\n"
                `shouldLexParse` sAssi "x" (eCall (eaId "f") [eaId "a", eaId "b"])
            it "missing expression" $
                lexParse pStatement "myvar =;"
                `shouldFailWith` err 2 (
                    utok (withPos 1 8 1 9 1 (Control Semicolon))
                    <> elabel "expression"
                )


    describe "block expressions" $ do
        it "empty block" $
            parseAndLex pBlockExpression "{}"
            `shouldLexParse` BlockExpression []
        it "empty block with linebreaks" $
            parseAndLex pBlockExpression "{\n\n\n}"
            `shouldLexParse` BlockExpression []
        it "block with a vdecl and semicolon" $
            parseAndLex pBlockExpression "{i32 a;}"
            `shouldLexParse` BlockExpression [sDecl (tId "i32") (vId "a") Nothing]
        it "block with a vdecl and semicolon with linebreaks" $
            parseAndLex pBlockExpression "{i32 a;\n\n}"
            `shouldLexParse` BlockExpression [sDecl (tId "i32") (vId "a") Nothing]
        it "block with a vdecl and linebreaks" $
            parseAndLex pBlockExpression "{i32 a\n\n}"
            `shouldLexParse` BlockExpression [sDecl (tId "i32") (vId "a") Nothing]
        it "block containing an expression an expression" $
            parseAndLex pExpression "{a;}"
            `shouldLexParse` eBlk [ sExpr $ eaId "a"]
        it "block containing var assignement and while loop" $
            parseAndLex pExpression "\
                \{\n\
                \   i32 a = 5\n\
                \   while (a < 7) {\n\
                \       a = a + 1\n\
                \   }\n\
                \   return a\n\
                \}"
            `shouldLexParse` eBlk
                [ sDecl (tId "i32") (vId "a") (Just $ eaInt 5)
                , sExpr $ eWhile
                    (eoLt (eaId "a") (eaInt 7))
                    (eBlk [
                        sAssi "a" (eoAdd (eaId "a") (eaInt 1))
                    ])
                , sRet $ eaId "a"
                ]
        it "fail with missing end of statement" $
            lexParse pBlockExpression "{i32 a = 4}"
            `shouldFailWith` err 5 (
                utok (withPos 1 11 1 12 1 (Control CloseBrace))
                <> etok (liftMyToken $ Control LineBreak)
                <> etok (liftMyToken $ Control Semicolon)
            )

    describe "parse entire functions" $ do
        it "add function" $
            parseAndLex pFunction
                "fun my_add(i32 a, i32 b): i32\n\
                \{\n\
                \    i32 result = a + b\n\
                \\n\
                \    return result\n\
                \}\n"
            `shouldLexParse` Function
                "my_add"
                [vdecl "i32" "a", vdecl "i32" "b"]
                (Just (tId "i32"))
                (BlockExpression
                    [ sDecl
                        (tId "i32")
                        (vId "result")
                        (Just $ eoAdd (eaId "a") (eaId "b"))
                    , sRet (eaId "result")
                    ]
                )
        it "function with no parameter list" $
            parseAndLex pFunction "fun f {}"
            `shouldLexParse` fn "f" [] Nothing []
        it "multiline function parameter list" $
            parseAndLex pFunction
                "fun f(\n\
                \   i32 a,\n\
                \   bool b,\n\
                \)\n\
                \{}\n"
            `shouldLexParse` fn "f" [vdecl "i32" "a", vdecl "bool" "b"] Nothing []
        it "function with wrong parameter list (fail)" $
            lexParse pFunction "fun myfunc(i32 a, {}"
            `shouldFailWith` err 6 (
                utok (withPos 1 19 1 20 1 (Control OpenBrace))
                <> etok (liftMyToken $ Control CloseParen)
                <> elabel "variable declaration"
            )
        it "function with wrong parameter list2 (fail)" $
            lexParse pFunction "fun myfunc(i32 , bool) {}"
            `shouldFailWith` err 4 (
                utok (withPos 1 16 1 17 1 (Control Comma))
                <> elabel "variable identifier"
            )
        it "function with wrong parameter list2 (fail)" $
            lexParse pFunction "fun myfunc(i32 a, bool) {}"
            `shouldFailWith` err 7 (
                utok (withPos 1 23 1 24 1 (Control CloseParen))
                <> elabel "variable identifier"
            )

        it "main function" $
            parseAndLex pMainFunction "main() {}"
            `shouldLexParse` MainFunction [] (BlockExpression [])
        it "main function with no param list" $
            parseAndLex pMainFunction "main {}"
            `shouldLexParse` MainFunction [] (BlockExpression [])

        it "parse program" $
            parseAndLex pProgram
                "fun somefunc() {}\n\
                \fun otherfunc(bool a){}\n\
                \\n\
                \main()\n\
                \{\n\
                \}\n\
                \fun lastfunc() {}\n"
            `shouldLexParse` Program
                (fnMain [] [])
                [ fn "somefunc" [] Nothing []
                , fn "otherfunc" [vdecl "bool" "a"] Nothing []
                , fn "lastfunc" [] Nothing []
                ]
