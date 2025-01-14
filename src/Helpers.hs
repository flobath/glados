module Helpers (
    comb,
    (<||>),
    (<<|>>),
    (?:),
    ffmap,
    (<:>),
) where

import Control.Applicative ( Alternative((<|>)) )

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

ffmap :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
ffmap = fmap . fmap
