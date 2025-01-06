module Parser.WithPos (
    WithPos(..),
    withPos,
    withPosSourced,
) where

import Text.Megaparsec(SourcePos(..), mkPos)

data WithPos a = WithPos
    { startPos :: SourcePos
    , endPos :: SourcePos
    , tokenLength :: Int
    , tokenVal :: a
    } deriving (Eq, Ord, Show)

withPos :: Int -> Int -> Int -> Int -> Int -> a -> WithPos a
withPos = withPosSourced ""

withPosSourced :: String -> Int -> Int -> Int -> Int -> Int -> a -> WithPos a
withPosSourced filename startLine startCol endLine endCol len val = WithPos
    { startPos = SourcePos
        { sourceName = filename
        , sourceLine = mkPos startLine
        , sourceColumn = mkPos startCol
        }
    , endPos = SourcePos
        { sourceName = filename
        , sourceLine = mkPos endLine
        , sourceColumn = mkPos endCol
        }
    , tokenLength = len
    , tokenVal = val
    }
