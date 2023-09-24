module Automata.DFA (DFA (..), eval, normalize) where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

data DFA state input = DFA state [(state, input, state)] [state] [input] deriving (Show)

eval :: (Ord a, Ord b) => DFA a b -> [b] -> Bool
eval (DFA initial transitions accept _) word = foldr transFunc initial word `elem` accept
  where transFunc input state = fromJust (M.lookup (state, input) transMap)
        (states, inputs, results) = unzip3 transitions
        transMap = M.fromList (zip (zip states inputs) results)

normalize :: (Ord a) => DFA a b -> DFA Int b
normalize (DFA initial transitions accept alphabet) = DFA (replace initial) (zip3 indices inputs (map replace results)) (map replace accept) alphabet
  where indices = concatMap (replicate (length alphabet)) [0..]
        (states, inputs, results) = unzip3 transitions
        dict = M.fromList (zip states indices)
        replace x = fromJust (M.lookup x dict)

minimize :: DFA a b -> DFA Int b
minimize = undefined

necessarilyDistinctStates :: (Ord a) => DFA a b -> S.Set (S.Set a)
necessarilyDistinctStates dfa = S.difference (S.difference (cartesianProduct states) (cartesianProduct nonAcceptingStates)) (cartesianProduct acceptingStates)
  where states = getStates dfa
        acceptingStates = getAcceptStates dfa
        nonAcceptingStates = S.difference states acceptingStates

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

cartesianProduct :: (Ord a) => S.Set a -> S.Set (S.Set a)
cartesianProduct set = S.map tupleToSet (S.cartesianProduct set set)
  where tupleToSet (a, b) = S.fromList [a, b] 
