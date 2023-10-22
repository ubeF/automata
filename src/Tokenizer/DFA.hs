module Tokenizer.DFA (DFA (..), step, compile, isAccepting, isJunked) where

import qualified Automata.DFA as D
import qualified Automata.NFA as N
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

optimize :: (Ord a, Ord b, Bounded a, Enum a) => D.DFA a b -> DFA a b
optimize dfa = DFA {
    transition = func
  , current = D.initial dfa
  , accept = S.fromList . D.accept $ dfa
  , junk = junkState
  }
  where junkState = fromMaybe (head . filter (not . (`elem` D.states dfa)) $ [minBound..]) (D.findJunkState dfa)
        func state input
          | input `elem` D.alphabet dfa = D.getTransitionFunction dfa state input
          | otherwise = junkState
