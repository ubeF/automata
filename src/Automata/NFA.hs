module Automata.NFA (NFA (..), Alphabet (..)) where

import qualified Data.Set as S
import Data.List

class Alphabet a where
  epsilon :: a

instance Alphabet Char where
  epsilon = toEnum 0

data NFA state input = NFA state [(state, input, state)] (state -> Bool)

epsilonClosure :: (Alphabet a, Eq a, Ord b) => [(b, a, b)] -> b -> S.Set b
epsilonClosure transitions = go
  where eTransitions = filter (\(_,x,_) -> x==epsilon) transitions
        transFunc state = S.fromList . map (\(_,_,x) -> x) . filter (\(x,_,_) -> x==state) $ eTransitions
        go state = S.unions . S.insert (S.singleton state) . S.map go . transFunc $ state

expandState :: (Alphabet a, Eq a, Ord b) => [(b, a, b)] -> a -> b -> S.Set b
expandState transitions input state = S.union newStates closures
  where newStates = S.fromList . map (\(_,_,z) -> z) . filter (\(x,y,_) -> x==state && input==y) $ transitions
        closures = foldr S.union S.empty . S.map (epsilonClosure transitions) $ newStates

expandSet :: (Alphabet a, Eq a, Ord b) => [(b, a, b)] -> S.Set b -> a -> S.Set b
expandSet transitions states input = foldr S.union S.empty . S.map (expandState transitions input) $ states

potentiateStates :: (Alphabet a, Eq a, Ord b) => [(b, a, b)] -> [a] -> S.Set b -> [(S.Set b, a, S.Set b)]
potentiateStates transitions alphabet = go []
  where go trans states = go newTransitions (head . filter (not . (`elem` done)) $ queue)
          where newResults = map (expandSet transitions states) alphabet
                newTransitions = trans ++ zipWith ((,,) states) alphabet newResults
                (done, _, queue) = unzip3 newTransitions

-- toDFA :: NFA a b -> DFA.DFA Int b
-- toDFA (NFA initial transition accept) = DFA.DFA newInitial newTransition newAccept
--   where tmpInitial = epsilonClosure transition initial

-- epsilonClosure transition start = go start
--   where go state = S.union newStates (foldr (S.union . go) S.empty newStates)
--           where newStates = transition state epsilon

testTransition :: [(Int, Char, Int)]
testTransition = [
    (0, epsilon, 1)
  , (0, epsilon, 2)
  , (1, 'b', 3)
  , (2, 'a', 4)
  , (3, 'a', 5)
  , (4, 'b', 6)
  , (5, epsilon, 7)
  , (6, epsilon, 7)
  , (7, epsilon, 0)
  ]