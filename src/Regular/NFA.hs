module Regular.NFA (NFA (..), Transition, toDFA, getTransitionFunction, getAcceptFunction) where


import qualified Data.Set as S
import qualified Regular.DFA as D
import qualified Data.Map as M


data NFA a b = NFA {
    states :: [a]
  , alphabet :: [b]
  , transitions :: [(a, Maybe b, a)]
  , initial :: a
  , accept :: [a]
} deriving (Show)

type Transition state input = state -> Maybe input -> [state]


getTransitionFunction:: (Ord state, Ord input) => NFA state input -> Transition state input
getTransitionFunction nfa = function
  where (stateInputs, inputs, _) = unzip3 . transitions $ nfa
        resultLists = zipWith (\a b -> map (\(_, _, z) -> z) . filter (\(x, y, _) -> a==x && b==y) $ transitions nfa) stateInputs inputs
        valMap = M.fromList (zip (zip stateInputs inputs) resultLists)
        function state input = case M.lookup (state, input) valMap of
          Nothing -> []
          (Just val) -> val

getAcceptFunction :: (Eq state) => NFA state input -> (state -> Bool)
getAcceptFunction nfa = (`elem` accept nfa)

getEpsilonClosure :: (Ord input, Ord state) => NFA state input -> (state -> S.Set state)
getEpsilonClosure nfa state = S.unions . (S.singleton state :) . map (getEpsilonClosure nfa) . epsilonTransition $ state
  where epsilonTransition s = getTransitionFunction nfa s Nothing 

getSetTransitionFunction ::(Ord input, Ord state) => NFA state input -> (S.Set state -> input -> S.Set state)
getSetTransitionFunction nfa = func
  where epsilonClosure = getEpsilonClosure nfa
        transition symbol state = (getTransitionFunction nfa) state (Just symbol)
        func states symbol = S.unions . S.map (S.unions . map epsilonClosure . transition symbol) $ states

potentiateStates :: (Ord input, Ord state) => NFA state input -> M.Map (S.Set state) [(input, S.Set state)]
potentiateStates nfa = go start M.empty
  where trans = getSetTransitionFunction nfa
        start = getEpsilonClosure nfa $ initial nfa
        go states dict
          | M.member states dict = dict
          | otherwise = foldr go newDict results
            where results = map (trans states) . alphabet $ nfa
                  newDict = M.insert states (zip (alphabet nfa) results) dict

toDFA :: (Ord input, Ord state) => NFA state input -> D.DFA Int input
toDFA nfa = D.normalize $ D.DFA {
    D.alphabet = alphabet nfa
  , D.transitions = trans
  , D.accept = filter (\x -> not $ S.disjoint x (S.fromList . accept $ nfa)) . M.keys $ dict
  , D.initial = getEpsilonClosure nfa (initial nfa)
  , D.states = M.keys dict
  }
  where dict = potentiateStates nfa
        trans = concatMap (\(x, tuples) -> map (\(y, z) -> (x, y, z)) tuples) . M.toList $ dict
        
