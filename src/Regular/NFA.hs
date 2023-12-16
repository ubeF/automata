module Regular.NFA (NFA (..), toDFA, eval) where


import qualified Data.Set as S
import qualified Regular.DFA as D
import qualified Data.Map as M
import Graph (Graph, fromFunction, toList, vertices)


data NFA a b = NFA {
    states :: [a]
  , alphabet :: [b]
  , transitions :: [(a, Maybe b, a)]
  , initial :: a
  , accept :: [a]
} deriving (Show)


transitionFunc :: (Ord state, Ord input) => NFA state input -> (state -> Maybe input -> [state])
transitionFunc nfa = function
  where (stateInputs, inputs, _) = unzip3 . transitions $ nfa
        resultLists = zipWith (\a b -> map (\(_, _, z) -> z) . filter (\(x, y, _) -> a==x && b==y) $ transitions nfa) stateInputs inputs
        valMap = M.fromList (zip (zip stateInputs inputs) resultLists)
        function state input = case M.lookup (state, input) valMap of
          Nothing -> []
          (Just val) -> val

epsilonClosure :: (Ord input, Ord state) => NFA state input -> (state -> S.Set state)
epsilonClosure nfa state = S.unions . (S.singleton state :) . map (epsilonClosure nfa) . epsilonTransition $ state
  where epsilonTransition s = transitionFunc nfa s Nothing 

linearTransitionFunc ::(Ord input, Ord state) => NFA state input -> (S.Set state -> input -> S.Set state)
linearTransitionFunc nfa = func
  where closure = epsilonClosure nfa
        transition symbol state = (transitionFunc nfa) state (Just symbol)
        func stateSet symbol = S.unions . S.map (S.unions . map closure . transition symbol) $ stateSet

potentiateStates :: (Ord input, Ord state) => NFA state input -> Graph (S.Set state) input
potentiateStates nfa = fromFunction trans start (alphabet nfa)
  where trans = linearTransitionFunc nfa
        start = epsilonClosure nfa $ initial nfa

toDFA :: (Ord input, Ord state) => NFA state input -> D.DFA Int input
toDFA nfa = D.normalize $ D.DFA {
    D.alphabet = alphabet nfa
  , D.transitions = toList graph
  , D.accept = filter (\x -> not $ S.disjoint x (S.fromList . accept $ nfa)) . vertices $ graph
  , D.initial = epsilonClosure nfa (initial nfa)
  , D.states = vertices graph
  }
  where graph = potentiateStates nfa

eval :: (Ord state, Ord input) => NFA state input -> [input] -> Bool
eval nfa = isAccepted . foldr trans start 
  where trans = flip $ linearTransitionFunc nfa
        isAccepted = not . S.disjoint (S.fromList . accept $ nfa)
        start = epsilonClosure nfa (initial nfa)
