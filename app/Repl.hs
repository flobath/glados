{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Repl (
    ifTTY,
    ReplState(ReplState),
    replRead,
    replReadConcat,
    replParse,
    replPrint,
    replEval,
    replLoop,
) where

import Data.Text as T ( Text, append, null )
import Lib ( Environment, Expression(..), Primitive(..), evaluate )
import Control.Monad.Trans.State (StateT, get, put)
import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Data.Text.IO as T
import GHC.IO.Handle (isEOF, hIsTerminalDevice, hFlush)
import System.Exit (exitSuccess, exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Text.Megaparsec (runParser', stateInput, ParseErrorBundle (bundleErrors), ParseError (TrivialError), ErrorItem (EndOfInput))
import Parser (parseExpression)
import Megaparsec96 (initialState)
import Control.Composition ((-.))
import Data.Functor ((<&>))
import Control.Monad.Loops (whileM_)
import Control.Monad (when)

data ReplState = ReplState Text Environment

type State = StateT ReplState IO

isUtokEof :: ParseErrorBundle a b -> Bool
isUtokEof =  bundleErrors -. any _isUtokEof
    where
        _isUtokEof (TrivialError _ (Just EndOfInput) _) = True
        _isUtokEof _ = False

showExpr :: Expression -> String
showExpr (Operation _ _) = "#<procedure>\n"
showExpr (Primitive p) = showPrimitive p

showPrimitive :: Primitive -> String
showPrimitive (Constant i) = show i ++ "\n"
showPrimitive (Boolean False) = "#f\n"
showPrimitive (Boolean True) = "#t\n"
showPrimitive (Text t) = show t ++ "\n"
showPrimitive Void = ""
showPrimitive _ = "#<procedure>\n"

exitWithErrorMessage :: String -> IO a
exitWithErrorMessage mes = do
    hPutStrLn stderr mes
    exitWith $ ExitFailure 84

ifTTY :: IO () -> IO ()
ifTTY a = do
    act <- hIsTerminalDevice stdin
    when act a

replRead :: State ()
replRead = do
    (ReplState _ env) <- get
    line <- lift $ do
        end <- isEOF
        if end
            then exitSuccess
            else T.getLine
    put (ReplState line env)

replReadConcat :: State ()
replReadConcat = do
    (ReplState buf env) <- get
    line <- lift $ do
        end <- isEOF
        if end
            then exitWithErrorMessage "Exception in read: unexpected end-of-file"
            else T.getLine
    let buf' = buf `append` "\n" `append` line
    put (ReplState buf' env)

replParse :: State Expression
replParse = do
    (ReplState buf env) <- get
    let (pState, pRes) = runParser' parseExpression (initialState "" buf)
    case pRes of
        Right expr -> do
            put (ReplState (stateInput pState) env)
            return expr
        Left (isUtokEof -> True) -> replReadConcat >> replParse
        _ -> lift $ exitWithErrorMessage "Unexpected end of file"

replEval :: Expression -> State Expression
replEval expr = do
    (ReplState buf env) <- get
    case evaluate env expr of
        Left (env', expr') -> do
            put (ReplState buf env')
            return expr'
        Right _ -> lift $ exitWithErrorMessage "Evaluation error"

replPrint :: Expression -> State ()
replPrint = lift . putStr . showExpr

replLoop :: State ()
replLoop = do
    lift $ ifTTY $ putStr "> " >> hFlush stdout
    replRead
    whileM_ (get <&> (\(ReplState buf _) -> not $ T.null buf)) $
        replParse >>= replEval >>= replPrint
    replLoop
