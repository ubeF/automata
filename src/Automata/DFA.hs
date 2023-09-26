module Automata.DFA (DFA (..), eval, normalize, minimize) where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
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

minimize :: (Ord a, Ord b) => DFA a b -> DFA Int b
minimize dfa = normalize . mergeStates dfa . findNonDistinctPairs dfa . findNecessarilyDistinctPairs $ dfa

findNecessarilyDistinctPairs :: (Ord a) => DFA a b -> (S.Set (S.Set a), S.Set (S.Set a))
findNecessarilyDistinctPairs dfa = (potentialPairs, distinctPairs)
  where states = getStates dfa
        acceptingStates = getAcceptStates dfa
        nonAcceptingStates = S.difference states acceptingStates
        potentialPairs = S.union (cartesianProduct acceptingStates) (cartesianProduct nonAcceptingStates)
        distinctPairs = S.difference (cartesianProduct states) potentialPairs

findNonDistinctPairs :: (Ord a, Ord b) => DFA a b -> (S.Set (S.Set a), S.Set (S.Set a)) -> S.Set (S.Set a)
findNonDistinctPairs dfa (potential, distinct)
  | S.null newDistinct = potential
  | otherwise = findNonDistinctPairs dfa (S.difference potential newDistinct, S.union distinct newDistinct)
  where newDistinct = S.filter (any (`elem` distinct) . bulkTransitionSet (getAlphabet dfa)) potential
        transition = flip . getTransitionFunction $ dfa
        transitionSet set input = S.map (transition input) set 
        bulkTransitionSet inputs set = map (transitionSet set) inputs

mergeStates :: (Ord a, Ord b) => DFA a b -> S.Set (S.Set a) -> DFA (S.Set a) b
mergeStates (DFA initial transitions accept alphabet) pairs = DFA (transform initial) newTransitions (removeDuplicates . map transform $ accept) alphabet
  where transform x = foldr (\merge state -> if S.disjoint merge state then state else S.union merge state) (S.singleton x) pairs
        (states, inputs, results) = unzip3 transitions
        newTransitions = removeDuplicates (zip3 (map transform states) inputs (map transform results))


getTransitionFunction :: (Ord a, Ord b) => DFA a b -> (a -> b -> a)
getTransitionFunction (DFA _ transitions _ _) = func
  where func state input = fromJust (M.lookup (state, input) transMap)
        (states, inputs, results) = unzip3 transitions
        transMap = M.fromList (zip (zip states inputs) results)

getInitial :: DFA a b -> a
getInitial (DFA initial _ _ _) = initial

getAcceptFunction :: (Eq a) => DFA a b -> (a -> Bool)
getAcceptFunction (DFA _ _ accept _) = (`elem` accept)

getAcceptStates :: (Ord a) => DFA a b -> S.Set a
getAcceptStates (DFA _ _ accept _) = S.fromList accept

getStates :: (Ord a) => DFA a b -> S.Set a
getStates (DFA _ transitions _ _) = S.fromList states
  where (states, _, _) = unzip3 transitions

getAlphabet :: DFA a b -> [b]
getAlphabet (DFA _ _ _ alphabet) = alphabet

cartesianProduct :: (Ord a) => S.Set a -> S.Set (S.Set a)
cartesianProduct set = S.map tupleToSet (S.cartesianProduct set set)
  where tupleToSet (a, b) = S.fromList [a, b] 

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = S.toList . S.fromList