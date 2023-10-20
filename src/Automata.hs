{-# LANGUAGE FlexibleInstances #-}

module Automata (NFA, (<+>), star, epsilon, lit, times, range, makeAscii, ascii) where

import Automata.NFA (Alphabet, NFA (alphabet), normalize)
import Automata.Operators ( logicalOr, singleton, kleeneStar, emptyWord, concatenate )

instance (Alphabet a, Ord a) => Semigroup (NFA Int a) where
  (<>) a b = normalize $ concatenate a b

instance (Alphabet a, Ord a) => Monoid (NFA Int a) where
  mempty = emptyWord


(<+>) :: (Alphabet a, Ord a) => NFA Int a -> NFA Int a -> NFA Int a
(<+>) a b = normalize $ logicalOr a b

lit :: input -> NFA Int input
lit = singleton

star :: (Alphabet a) => NFA Int a -> NFA Int a
star = normalize . kleeneStar

epsilon :: (Alphabet a) => NFA Int a
epsilon = emptyWord

range :: (Alphabet a, Ord a) => [a] -> NFA Int a
range = foldr (<+>) epsilon . map lit

times :: (Alphabet a, Ord a) => (Int, Int) -> NFA Int a -> NFA Int a
times (lower, upper) dfa = foldr (<+>) epsilon . map mconcat $ map (`replicate` dfa) [lower..upper]

ascii :: String
ascii = map toEnum [32..127]

makeAscii ::  NFA Int Char -> NFA Int Char
makeAscii dfa = dfa {alphabet=ascii}