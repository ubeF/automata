{-# LANGUAGE FlexibleInstances #-}

module Automata (NFA, (<+>), star, epsilon, lit, times, range, makeAscii) where

import Automata.NFA (NFA (alphabet), normalize)
import Automata.Operators ( logicalOr, singleton, kleeneStar, emptyWord, concatenate )

instance (Ord a) => Semigroup (NFA Int a) where
  (<>) a b = normalize $ concatenate a b

instance (Ord a) => Monoid (NFA Int a) where
  mempty = emptyWord


(<+>) :: (Ord a) => NFA Int a -> NFA Int a -> NFA Int a
(<+>) a b = normalize $ logicalOr a b

lit :: input -> NFA Int input
lit = singleton

star :: NFA Int a -> NFA Int a
star = normalize . kleeneStar

epsilon :: NFA Int a
epsilon = emptyWord

range :: (Ord a) => [a] -> NFA Int a
range = foldr (<+>) epsilon . map lit

times :: (Ord a) => (Int, Int) -> NFA Int a -> NFA Int a
times (lower, upper) dfa = foldr (<+>) epsilon . map mconcat $ map (`replicate` dfa) [lower..upper]

ascii :: String
ascii = map toEnum [32..127]

makeAscii ::  NFA Int Char -> NFA Int Char
makeAscii dfa = dfa {alphabet=ascii}