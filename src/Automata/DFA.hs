module Automata.DFA (DFA (..), eval, normalize) where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

data DFA state input = DFA state [(state, input, state)] [state] [input] deriving (Show)

eval :: (Ord a, Ord b) => DFA a b -> [b] -> Bool
eval dfa = getAcceptFunction dfa . foldr transFunc (getInitial dfa)
  where transFunc = flip . getTransitionFunction $ dfa

normalize :: (Ord a) => DFA a b -> DFA Int b
normalize (DFA initial transitions accept alphabet) = DFA (replace initial) (zip3 indices inputs (map replace results)) (map replace accept) alphabet
  where indices = concatMap (replicate (length alphabet)) [0..]
        (states, inputs, results) = unzip3 transitions
        dict = M.fromList (zip states indices)
        replace x = fromJust (M.lookup x dict)

minimize :: DFA a b -> DFA Int b
minimize = undefined

-- findMergableStates :: DFA a b -> S.Set (S.Set a)
-- findMergableStates (DFA intial transitions accept alphabet) = undefined
--   where pairs = S.cartesianProduct (getStates )

getTransitionFunction :: (Ord a, Ord b) => DFA a b -> (a -> b -> a)
getTransitionFunction (DFA _ transitions _ _) = func
  where func state input = fromJust (M.lookup (state, input) transMap)
        (states, inputs, results) = unzip3 transitions
        transMap = M.fromList (zip (zip states inputs) results)

getInitial :: DFA a b -> a
getInitial (DFA initial _ _ _) = initial

getAcceptFunction :: (Eq a) => DFA a b -> (a -> Bool)
getAcceptFunction (DFA _ _ accept _) = (`elem` accept)

getStates :: (Ord a) => DFA a b -> S.Set a
getStates (DFA _ transitions _ _) = S.fromList states
  where (states, _, _) = unzip3 transitions