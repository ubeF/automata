module Tokenizer.DFA (DFA (..), step, compile, isAccepting, isJunked) where

import qualified Regular.DFA as D
import qualified Regular.Minimal as MD
import qualified Regular.NFA as N
import qualified Data.Set as S
import Data.Maybe

data DFA a b = DFA {
  -- DFA optimized for usage in tokenizer
    transition :: a -> b -> a
  , current :: a
  , accept :: S.Set a
  , junk :: a
}

compile :: (Ord a, Ord b) => N.NFA a b -> DFA Int b
compile = optimize . D.minimize . N.toDFA

isAccepting :: (Ord a) => DFA a b -> Bool
isAccepting dfa = S.member (current dfa) (accept dfa)

isJunked :: (Ord a) => DFA a b -> Bool
isJunked dfa = current dfa == junk dfa 

step :: DFA a b -> b -> DFA a b
step dfa x = dfa { current = newState }
  where newState = transition dfa (current dfa) x

optimize :: (Ord a, Ord b, Bounded a, Enum a) => MD.DFA a b -> DFA a b
optimize dfa = DFA {
    transition = func
  , current = MD.initial dfa
  , accept = S.fromList . MD.accept $ dfa
  , junk = junkState
  }
  where junkState = fromMaybe (head . filter (not . (`elem` MD.states dfa)) $ [minBound..]) (MD.findJunkState dfa)
        func state input
          | input `elem` MD.alphabet dfa = MD.transition dfa state input
          | otherwise = junkState
