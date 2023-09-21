module Automata.NFA (NFA (..), Alphabet (..)) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

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
  where go trans state = case todo of
          [] -> newTransitions
          x:_ -> go newTransitions x
          where newTransitions = trans ++ helper transitions alphabet state
                (done, _, results) = unzip3 newTransitions
                todo = filter (not . (`elem` done)) results

helper :: (Alphabet a, Eq a, Ord b) => [(b, a, b)] -> [a] -> S.Set b -> [(S.Set b, a, S.Set b)] 
helper transitions alphabet state = zipWith ((,,) state) alphabet results
  where results = map (expandSet transitions state) alphabet

normalize :: Ord b => [(S.Set b, a, S.Set b)] -> [(Int, a, Int)]
normalize transitions = zip3 indices alphabet (map (\x -> fromJust (M.lookup x dict)) vals)
  where indices = [0..]
        (keys, alphabet, vals) = unzip3 transitions
        dict = M.fromList (zip keys indices)
        
-- toDFA :: NFA a b -> DFA.DFA Int b
-- toDFA (NFA initial transition accept) = DFA.DFA newInitial newTransition newAccept
--   where tmpInitial = epsilonClosure transition initial


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