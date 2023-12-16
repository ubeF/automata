{-# LANGUAGE FlexibleInstances #-}

module Regular (NFA, (<+>), star, epsilon, lit, times, range, word) where


import Regular.NFA (NFA)
import Regular.NFA.Operators ( logicalOr, singleton, kleeneStar, emptyWord, concatenate, normalize )


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

word :: (Ord a) => [a] -> NFA Int a
word = mconcat . map lit

times :: (Ord a) => (Int, Int) -> NFA Int a -> NFA Int a
times (lower, upper) dfa = foldr (<+>) epsilon . map mconcat $ map (`replicate` dfa) [lower..upper]
