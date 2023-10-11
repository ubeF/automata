module Automata.DFA (DFA (..), eval, normalize, minimize) where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

data DFA a b = DFA {
    states :: [a]
  , alphabet :: [b]
  , transitions :: [(a, b, a)]
  , initial :: a
  , accept :: [a]
}

getTransitionFunction :: (Ord a, Ord b) => DFA a b -> (a -> b -> a)
getTransitionFunction dfa = func
  where func state input = fromJust (M.lookup (state, input) transMap)
        (states, inputs, results) = unzip3 $ transitions dfa
        transMap = M.fromList (zip (zip states inputs) results)

getAcceptFunction :: (Eq a) => DFA a b -> (a -> Bool)
getAcceptFunction dfa = (`elem` accept dfa)

eval :: (Ord a, Ord b) => DFA a b -> [b] -> Bool
eval dfa = getAcceptFunction dfa . foldr transFunc (initial dfa)
  where transFunc = flip . getTransitionFunction $ dfa

transformStates :: (a -> b) -> DFA a c -> DFA b c
transformStates f dfa = dfa { 
      states=newStates
    , initial=newInitial
    , accept=newAccept
    , transitions=newTransitions 
    }
  where newInitial = f . initial $ dfa
        newStates = map f. states $ dfa
        newAccept = map f . accept $ dfa
        (a, b, c) = unzip3 . transitions $ dfa
        newTransitions = zip3 (map f a) b (map f c)

normalize :: (Ord a) => DFA a b -> DFA Int b
normalize dfa = transformStates func dfa
  where valMap = M.fromList $ zip (states dfa) [1..]
        func x = fromJust $ M.lookup x valMap

minimize :: (Ord a, Ord b) => DFA a b -> DFA Int b
minimize dfa = removeDuplicateStates . normalize . mergeStates dfa . findNonDistinctPairs dfa . findNecessarilyDistinctPairs $ dfa

findNecessarilyDistinctPairs :: (Ord a) => DFA a b -> (S.Set (S.Set a), S.Set (S.Set a))
findNecessarilyDistinctPairs dfa = (potentialPairs, distinctPairs)
  where allStates = S.fromList . states $ dfa
        acceptingStates = S.fromList . accept $ dfa
        nonAcceptingStates = S.difference allStates acceptingStates
        potentialPairs = S.union (cartesianProduct acceptingStates) (cartesianProduct nonAcceptingStates)
        distinctPairs = S.difference (cartesianProduct allStates) potentialPairs

findNonDistinctPairs :: (Ord a, Ord b) => DFA a b -> (S.Set (S.Set a), S.Set (S.Set a)) -> S.Set (S.Set a)
findNonDistinctPairs dfa (potential, distinct)
  | S.null newDistinct = potential
  | otherwise = findNonDistinctPairs dfa (S.difference potential newDistinct, S.union distinct newDistinct)
  where newDistinct = S.filter (any (`elem` distinct) . bulkTransitionSet (alphabet dfa)) potential
        transition = flip . getTransitionFunction $ dfa
        transitionSet set input = S.map (transition input) set
        bulkTransitionSet inputs set = map (transitionSet set) inputs

mergeStates :: (Ord a) => DFA a b -> S.Set (S.Set a) -> DFA (S.Set a) b
mergeStates dfa pairs = transformStates f dfa
  where f x = foldr (\merge state -> if S.disjoint merge state then state else S.union merge state) (S.singleton x) pairs

cartesianProduct :: (Ord a) => S.Set a -> S.Set (S.Set a)
cartesianProduct set = S.map tupleToSet (S.cartesianProduct set set)
  where tupleToSet (a, b) = S.fromList [a, b]

removeDuplicateStates :: (Ord a, Ord b) => DFA a b -> DFA a b
removeDuplicateStates dfa = dfa { accept=newAccept, states=newStates, transitions=newTransitions }
  where newStates = removeDuplicates . states $ dfa
        newAccept = removeDuplicates . accept $ dfa
        newTransitions = removeDuplicates . transitions $ dfa

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = S.toList . S.fromList