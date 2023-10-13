{-# LANGUAGE FlexibleInstances #-}

module Automata where

import Automata.NFA
import Automata.Operators

(<+>) :: (Alphabet a, Ord a) => NFA Int a -> NFA Int a -> NFA Int a
(<+>) a b = normalize $ logicalOr a b

lit :: input -> NFA Int input
lit = singleton

star :: (Alphabet a) => NFA Int a -> NFA Int a
star = normalize . kleeneStar

epsilon :: (Alphabet a) => NFA Int a
epsilon = emptyWord

instance (Alphabet a, Ord a) => Semigroup (NFA Int a) where
  (<>) a b = normalize $ concatenate a b

instance (Alphabet a, Ord a) => Monoid (NFA Int a) where
  mempty = emptyWord


