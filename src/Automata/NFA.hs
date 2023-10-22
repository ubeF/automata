module Automata.NFA (NFA (..), eval, toDFA, normalize, transformStates) where

import qualified Data.Set as S
import qualified Automata.DFA as DFA
import qualified Data.Map as M
import Data.Maybe

data NFA a b = NFA {
    states :: [a]
  , alphabet :: [b]
  , transitions :: [(a, Maybe b, a)]
  , initial :: a
  , accept :: [a]
} deriving (Show)

transformStates :: (a -> b) -> NFA a c -> NFA b c
transformStates f nfa = nfa { 
      states=newStates
    , initial=newInitial
    , accept=newAccept
    , transitions=newTransitions 
    }
  where newInitial = f . initial $ nfa
        newStates = map f. states $ nfa
        newAccept = map f . accept $ nfa
        (a, b, c) = unzip3 . transitions $ nfa
        newTransitions = zip3 (map f a) b (map f c)

normalize :: (Ord a) => NFA a b -> NFA Int b
normalize nfa = transformStates func nfa
  where valMap = M.fromList $ zip (states nfa) [1..]
        func x = fromJust $ M.lookup x valMap

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

data Result = Success | Failure | Continue deriving (Show)

data Config state input = Config state [input]

eval :: (Ord state, Ord input) => NFA state input -> [input] -> Bool
eval nfa input = run [Config (initial nfa) input]
  where run vals = case evaluateConfigs (getAcceptFunction nfa) vals of
                        Success -> True
                        Failure -> False
                        Continue -> run . concatMap (generateConfigs . getTransitionFunction $ nfa) $ vals

generateConfigs :: Transition state input -> Config state input -> [Config state input]
generateConfigs transition (Config state word) = configs <> epsilonConfigs
  where gen input rest = map (`Config` rest) (transition state input)
        configs
          | null word = []
          | otherwise = gen (Just . head $ word) (tail word)
        epsilonConfigs = gen Nothing word

evaluateConfigs :: (state -> Bool) -> [Config state input] -> Result
evaluateConfigs _ [] = Failure
evaluateConfigs acc configs
  | any (evaluateConfig acc) configs = Success
  | otherwise = Continue

evaluateConfig :: (state -> Bool) -> Config state input -> Bool
evaluateConfig acc (Config state word)
  | null word = acc state
  | otherwise = False

toDFA :: (Ord state, Ord input) => NFA state input -> DFA.DFA Int input
toDFA nfa = DFA.normalize (DFA.DFA  newStates (alphabet nfa) newTransitions newInitial newAccept)
  where transition = getTransitionFunction nfa
        newInitial = epsilonClosure transition (initial nfa)
        newTransitions = potentiateStates transition (alphabet nfa) newInitial
        (a, _, b) = unzip3 newTransitions
        newStates = S.toList $ S.union (S.fromList a) (S.fromList b)
        newAccept = filter (any (getAcceptFunction nfa)) . S.toList . S.fromList $ newStates

epsilonClosure :: (Eq input, Ord state) => Transition state input -> state -> S.Set state
epsilonClosure transition state = S.unions . S.insert (S.singleton state) . S.map (epsilonClosure transition) . S.fromList $ transition state Nothing

potentiateStates :: (Ord state, Eq input) => Transition state input -> [input] -> S.Set state -> [(S.Set state, input, S.Set state)]
potentiateStates transition alphabetVals = go []
  where go trans state = case todo of
          [] -> newTransitions
          x:_ -> go newTransitions x
          where newTransitions = trans ++ makeTransitions transition alphabetVals state
                (done, _, results) = unzip3 newTransitions
                todo = filter (not . (`elem` done)) results

makeTransitions :: (Ord state, Eq input) => Transition state input -> [input] -> S.Set state -> [(S.Set state, input, S.Set state)]
makeTransitions transition alphabetVals stateVals = zipWith ((,,) stateVals) alphabetVals results
  where results = map (transitionSet transition stateVals) alphabetVals

transitionSet :: (Ord state, Eq input) => Transition state input -> S.Set state -> input -> S.Set state
transitionSet transition stateVals input = S.union newStates closure
  where newStates = S.unions . S.map S.fromList . S.map (`transition` Just input) $ stateVals
        closure = S.unions . S.map (epsilonClosure transition) $ newStates
