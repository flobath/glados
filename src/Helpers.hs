module Helpers (
    comb,
    (<||>),
    (<<|>>),
    (?:),
    ffmap,
    (<:>),
    (>&<),
    orelse,
    exitWithErrorMessage,
    orExitWith,
    headOr,
    mapMToSnd,
) where

import Control.Applicative (Alternative((<|>)))
import Data.Bifunctor (Bifunctor(first))
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (stderr, hPutStrLn)

-- Helper function used to create combinator operators
-- See defintion of (<||>) and (<<|>>) for example uses
comb :: (b -> b -> b) -> (a -> b) -> (a -> b) -> (a -> b)
comb combinator f1 f2 input = f1 input `combinator` f2 input

-- Helper function to combine predicates with an OR:
-- this function satisfies (p1 <||> p2) x = p1 x || p2 x
(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<||>) = comb (||)

-- Combines two functions which return alternatives given
-- the same input type. Attempts the first then the second.
(<<|>>) :: Alternative f => (t -> f a) -> (t -> f a) -> (t -> f a)
(<<|>>) = comb (<|>)

-- Applies (:) to Functor-wrapped Lists
--
-- Examples:
-- - 1 ?: 2 ?: Just [3] = Just [1, 2, 3]
-- - 1 ?: Nothing = Nothing
-- - 42 ?: 7 ?: Left "no list" = Left "no list"
infixr 5 ?:
(?:) :: Functor f => a -> f [a] -> f [a]
(?:) x = fmap (x:)

-- Applies (:) to Applicative-wrapped elements and Lists
--
-- Examples:
-- - Just 5 <:> Just [4, 3] = Just [5, 4, 3]
-- - Nothing <:> Just [4, 3] = Nothing
-- - Just 5 <:> Nothing = Nothing
infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

-- Helper which maps over the error variant of an 'Either'
(>&<) :: Bifunctor p => p a c -> (a -> b) -> p b c
(>&<) = flip first

ffmap :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
ffmap = fmap . fmap

orelse :: Monad m => Either t a -> (t -> m a) -> m a
orelse (Right b) _ = return b
orelse (Left a) f = f a

exitWithErrorMessage :: String -> IO a
exitWithErrorMessage msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

orExitWith :: (String -> Either String a) -> String -> IO a
orExitWith f msg = f msg `orelse` exitWithErrorMessage

headOr :: [a] -> b -> Either b a
headOr [] b = Left b
headOr (x:_) _ = Right x

-- Alternate version of mapM which also returns the values to be mapped
mapMToSnd :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
mapMToSnd f = foldr k (return [])
    where
        -- k :: a -> m [(a, b)] -> m [(a, b)]
        k a r = do { x <- f a; xs <- r; return ((a, x):xs) }
