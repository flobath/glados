{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module AlexToParsec (
    TokenStream(..),
) where

import Lexer (WithPos (tokenVal, tokenLength, endPos, startPos), Token (Control))
import Lexer.Tokens (Token, ControlSequence (LineBreak, Semicolon))
import Data.Text (Text)
import Text.Megaparsec (PosState(..))
import Data.Proxy (Proxy(..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Text.Megaparsec as MP
import qualified Data.List.NonEmpty as NE
import qualified Data.List as DL

-- This module is an adaptation of
-- https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams
-- for our token type. It needed some tweaks to work with `Text` instead of `String`

data TokenStream = TokenStream
    { myStreamInput :: Text -- for showing offending lines
    , unTokenStream :: [WithPos Token]
    }

instance MP.Stream TokenStream where
    type Token  TokenStream = WithPos Token
    type Tokens TokenStream = [WithPos Token]

    tokenToChunk Proxy x = [x]
    tokensToChunk Proxy xs = xs
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    take1_ (TokenStream _ []) = Nothing
    take1_ (TokenStream str (t:ts)) = Just
        ( t
        , TokenStream (T.drop (MP.tokensLength pxy (t:|[])) str) ts
        )
    takeN_ n (TokenStream str s)
        | n <= 0    = Just ([], TokenStream str s)
        | null s    = Nothing
        | otherwise =
            let (x, s') = splitAt n s
            in case NE.nonEmpty x of
            Nothing -> Just (x, TokenStream str s')
            Just nex -> Just (x, TokenStream (T.drop (MP.tokensLength pxy nex) str) s')
    takeWhile_ f (TokenStream str s) =
        let (x, s') = DL.span f s
        in case NE.nonEmpty x of
        Nothing -> (x, TokenStream str s')
        Just nex -> (x, TokenStream (T.drop (MP.tokensLength pxy nex) str) s')

instance MP.VisualStream TokenStream where
    showTokens Proxy = unwords
        . NE.toList
        . fmap (showMyToken . tokenVal)
    tokensLength Proxy xs = sum (tokenLength <$> xs)

instance MP.TraversableStream TokenStream where
    reachOffset o PosState{..} =
        ( Just (prefix ++ restOfLine')
        , PosState
            { pstateInput = TokenStream
                { myStreamInput = postStr
                , unTokenStream = post
                }
            , pstateOffset = max pstateOffset o
            , pstateSourcePos = newSourcePos
            , pstateTabWidth = pstateTabWidth
            , pstateLinePrefix = prefix
            }
        )
        where
        prefix =
            if sameLine
            then pstateLinePrefix ++ preLine'
            else preLine'
        sameLine = MP.sourceLine newSourcePos == MP.sourceLine pstateSourcePos
        newSourcePos =
            case post of
            [] -> case unTokenStream pstateInput of
                [] -> pstateSourcePos
                xs -> endPos (last xs)
            (x:_) -> startPos x
        (pre, post) = splitAt (o - pstateOffset) (unTokenStream pstateInput)
        (preStr, postStr) = T.splitAt tokensConsumed (myStreamInput pstateInput)
        preLine = T.reverse . T.takeWhile (/= '\n') . T.reverse $ preStr
        tokensConsumed =
            case NE.nonEmpty pre of
            Nothing -> 0
            Just nePre -> MP.tokensLength pxy nePre
        restOfLine = T.takeWhile (/= '\n') postStr

        preLine' = T.unpack preLine
        restOfLine' = T.unpack restOfLine

pxy :: Proxy TokenStream
pxy = Proxy

showMyToken :: Token -> String
showMyToken (Control LineBreak) = "linebreak"
showMyToken (Control Semicolon) = "';'"
showMyToken t = show t-- TODO: replace this by some pretty-print of tokens
