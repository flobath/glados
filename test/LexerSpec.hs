{-# LANGUAGE OverloadedStrings #-}

module LexerSpec (spec) where
import Test.Hspec (describe, shouldBe, it, Spec)
import Lexer (myScanTok, Token (Control))
import Lexer.Tokens (ControlSequence(LineBreak))

spec :: Spec
spec = do
    describe "Lex a linebreak" $ do
        it "single \\n" $
            myScanTok "\n" `shouldBe` Right [Control LineBreak]
