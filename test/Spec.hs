{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec.Megaparsec (shouldParse,
    succeedsLeaving,
    initialState,
    shouldFailOn,
    shouldSucceedOn,
    shouldFailWith,
    err,
    etok,
    ueof,
    elabel,
    )
import Test.Hspec (hspec, describe, it, shouldBe)
import Text.Megaparsec (parse, runParser')
import Parser.Internal
import Parser
import Lib (
    Primitive(Boolean, Constant, SymbolReference, SymbolList, Data, Function),
    Symbol (Symbol),
    Expression (Primitive, Operation),
    Operator (Unary, Binary, Ternary, Nary),
    Arguments (Pair, List, Triple, Single),
    Environment (Environment),
    addOperator,
    defineOperator,
    callOperator,
    ifOperator,
    eqOperator,
    multiplyOperator,
    lambdaOperator,
    subtractOperator,
    evaluate,
    defaultEnvironment,
    )
import qualified Data.Text
import Data.Text (Text)

testText :: Text -> (Text, Int)
testText t = (t, Data.Text.length t)

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
        it "fail on missing closing parenthesis" $ do
            let (t, my_text_length) = testText "(define abc 3"
            parse defineParser "" t
                `shouldFailWith` err my_text_length (ueof <> etok ')' <> elabel "digit")
        it "fail on missing expression" $ do
            let (t, my_text_length) = testText "(define abc (if"
            parse defineParser "" t
                `shouldFailWith` err my_text_length (ueof <> elabel "an expression")
    describe "parse function call" $ do
        it "simple call (myFunc 1 3)" $ do
            parse callParser "" "(myFunc 1 3)"
            `shouldParse` Operation callOperator (List
                [ Primitive $ SymbolReference $ Symbol "myFunc"
                , Primitive (Constant 1)
                , Primitive (Constant 3)
                ])

    describe "functional tests" $ do
        it "parse a factorial function (check for success)" $ do
            parse expressionParser ""
            `shouldSucceedOn` "(define fact (lambda (x) (if (eq? x 1) 1 (* x (fact (- x 1))))))"

        it "parse a factorial" $ do
            parse expressionParser ""
                "(define fact (lambda (x) (if (eq? x 1) 1 (* x (fact (- x 1))))))"
                `shouldParse` Operation defineOperator (Pair
                    (Primitive $ SymbolList [Symbol "fact"])
                    (Operation lambdaOperator (Pair
                        (Primitive $ SymbolList [Symbol "x"])
                        (Operation ifOperator (Triple
                            (Operation eqOperator (Pair
                                (Primitive $ SymbolReference $ Symbol "x")
                                (Primitive $ Constant 1)
                            ))
                            (Primitive $ Constant 1)
                            (Operation multiplyOperator (Pair
                                (Primitive $ SymbolReference $ Symbol "x")
                                (Operation callOperator (List
                                    [ Primitive $ SymbolReference $ Symbol "fact"
                                    , Operation subtractOperator (Pair
                                        (Primitive $ SymbolReference $ Symbol "x")
                                        (Primitive $ Constant 1)
                                    )
                                ]))
                            ))
                        ))
                    ))
                )

    describe "Primitive" $ do
        it "should show a constant correctly" $ do
            show (Constant 42) `shouldBe` "42"
        it "should show a boolean correctly" $ do
            show (Boolean True) `shouldBe` "True"
        it "should show a symbol reference correctly" $ do
            show (SymbolReference (Symbol "x")) `shouldBe` "[x]"
        it "should show a symbol list correctly" $ do
            show (SymbolList [Symbol "x", Symbol "y"]) `shouldBe` "(x y)"
        it "should show a data pair correctly" $ do
            show (Data [(Symbol "x", Constant 42)]) `shouldBe` "{x: 42}"
        it "should show a function correctly" $ do
            show (Function [Symbol "x"] (Primitive (Constant 42))) `shouldBe` "(Function with paramaters {x} evaluating 42)"

    describe "Operator" $ do
        it "should show a unary operator correctly" $ do
            show (Unary "-") `shouldBe` "unary operator '-'"
        it "should show a binary operator correctly" $ do
            show (Binary "+") `shouldBe` "binary operator '+'"
        it "should show a ternary operator correctly" $ do
            show (Ternary "if") `shouldBe` "ternary operator 'if'"
        it "should show an n-ary operator correctly" $ do
            show (Nary "") `shouldBe` "call operator"
            show (Nary "apply") `shouldBe` "n-ary operator 'apply'"

    describe "Expression" $ do
        it "should show a primitive expression correctly" $ do
            show (Primitive (Constant 42)) `shouldBe` "42"
        it "should show an operation with a single argument correctly" $ do
            show (Operation (Unary "-") (Single (Primitive (Constant 42)))) `shouldBe` "(Applying unary operator '-' to 42)"
        it "should show an operation with a pair of arguments correctly" $ do
            show (Operation (Binary "+") (Pair (Primitive (Constant 1)) (Primitive (Constant 2)))) `shouldBe` "(Applying binary operator '+' to 1 and 2)"
        it "should show an operation with a list of arguments correctly" $ do
            show (Operation (Nary "apply") (List [Primitive (Constant 1), Primitive (Constant 2)])) `shouldBe` "(Applying n-ary operator 'apply' to arguments 1 2)"

    describe "Environment" $ do
        it "should show an environment correctly" $ do
            let env = Environment [] [(Symbol "x", Primitive (Constant 42))]
            show env `shouldBe` "Environment with 0 operators defined and the following 1 symbols [(x,42)]"

    describe "evaluate" $ do
        it "should evaluate a constant correctly" $ do
            let env = defaultEnvironment
            evaluate env (Primitive (Constant 42)) `shouldBe` Left (env, Primitive (Constant 42))
        it "should evaluate a symbol reference correctly" $ do
            let env = Environment [] [(Symbol "x", Primitive (Constant 42))]
            evaluate env (Primitive (SymbolReference (Symbol "x"))) `shouldBe` Left (env, Primitive (Constant 42))
        it "should evaluate an addition operation correctly" $ do
            let env = defaultEnvironment
            evaluate env (Operation addOperator (Pair (Primitive (Constant 1)) (Primitive (Constant 2)))) `shouldBe` Left (env, Primitive (Constant 3))
