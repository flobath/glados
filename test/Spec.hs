import Test.Hspec.Megaparsec (shouldParse, succeedsLeaving, initialState, failsLeaving)
import Test.Hspec (hspec, describe, it)
import Text.Megaparsec (parse, runParser')
import Parser (booleanParser)
import Lib (Primitive(Boolean))

main :: IO ()
main = hspec $
    describe "parseBoolean" $ do
        it "parse a simple #t" $
            parse booleanParser "" "#t" `shouldParse` Boolean True
        it "parse a simple #f" $
            parse booleanParser "" "#f" `shouldParse` Boolean False

        it "consume trailing whitespace" $
            runParser' booleanParser (initialState "#t  \n   \t(abc") `succeedsLeaving` "(abc"

        it "fail with #fabcd" $
            runParser' booleanParser (initialState "#fabcd") `failsLeaving` "#fabcd"

        it "stops at opening parenthesis" $
            runParser' booleanParser (initialState "#f(hi)") `succeedsLeaving` "(hi)"
        it "stops at opening quote" $
            runParser' booleanParser (initialState "#f\"hi\"") `succeedsLeaving` "\"hi\""
        it "stops at following boolean literal" $
            runParser' booleanParser (initialState "#f#t") `succeedsLeaving` "#t"
