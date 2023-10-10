module Automata.NFA (NFA (..), Alphabet (..), toDFA, eval, getInitial, getAcceptStates) where

import qualified Data.Set as S
import qualified Automata.DFA as DFA
import qualified Data.Map as M

class Alphabet a where
  epsilon :: a

instance Alphabet Char where
  epsilon = toEnum 0

data NFA state input = NFA state [(state, input, state)] [state] [input] deriving (Show)

type Transition state input = state -> input -> [state]

getTransitionFunction:: (Ord state, Ord input) => NFA state input -> Transition state input
getTransitionFunction (NFA _ transitions _ _) = function
  where (states, inputs, _) = unzip3 transitions
        resultLists = zipWith (\a b -> map (\(_, _, z) -> z) . filter (\(x, y, _) -> a==x && b==y) $ transitions) states inputs
        valMap = M.fromList (zip (zip states inputs) resultLists)
        function state input = case M.lookup (state, input) valMap of
          Nothing -> []
          (Just val) -> val

getAcceptFunction :: (Eq state) => NFA state input -> (state -> Bool)
getAcceptFunction (NFA _ _ accept _) = (`elem` accept)

getAcceptStates :: NFA state input -> [state]
getAcceptStates (NFA _ _ x _) = x

getInitial :: NFA state input -> state
getInitial (NFA x _ _ _) = x

getAlphabet :: NFA state input -> [input]
getAlphabet (NFA _ _ _ xs) = xs

data Result = Success | Failure | Continue deriving (Show)

data Config state input = Config state [input]

eval :: (Ord state, Ord input, Alphabet input) => NFA state input -> [input] -> Bool
eval nfa input = run [Config (getInitial nfa) input]
  where run vals = case evaluateConfigs (getAcceptFunction nfa) vals of
                        Success -> True
                        Failure -> False
                        Continue -> run . concatMap (generateConfigs . getTransitionFunction $ nfa) $ vals

generateConfigs :: Alphabet input => Transition state input -> Config state input -> [Config state input]
generateConfigs transition (Config state word) = configs <> epsilonConfigs
  where gen input rest = map (`Config` rest) (transition state input)
        configs
          | null word = []
          | otherwise = gen (head word) (tail word)
        epsilonConfigs = gen epsilon word

evaluateConfigs :: (state -> Bool) -> [Config state input] -> Result
evaluateConfigs _ [] = Failure
evaluateConfigs accept configs
  | any (evaluateConfig accept) configs = Success
  | otherwise = Continue

evaluateConfig :: (state -> Bool) -> Config state input -> Bool
evaluateConfig accept (Config state word)
  | null word = accept state
  | otherwise = False

toDFA :: (Ord state, Alphabet input, Ord input) => NFA state input -> DFA.DFA Int input
toDFA nfa = DFA.normalize (DFA.DFA  newInitial newTransitions newAccept alphabet)
  where alphabet = getAlphabet nfa
        transition = getTransitionFunction nfa
        newInitial = epsilonClosure transition (getInitial nfa)
        newTransitions = potentiateStates transition alphabet newInitial
        (newStates, _, _) = unzip3 newTransitions
        newAccept = filter (any (getAcceptFunction nfa)) . S.toList . S.fromList $ newStates

epsilonClosure :: (Alphabet input, Eq input, Ord state) => Transition state input -> state -> S.Set state
epsilonClosure transition state = S.unions . S.insert (S.singleton state) . S.map (epsilonClosure transition) . S.fromList $ transition state epsilon

potentiateStates :: (Ord state, Alphabet input, Eq input) => Transition state input -> [input] -> S.Set state -> [(S.Set state, input, S.Set state)]
potentiateStates transition alphabet = go []
  where go trans state = case todo of
          [] -> newTransitions
          x:_ -> go newTransitions x
          where newTransitions = trans ++ makeTransitions transition alphabet state
                (done, _, results) = unzip3 newTransitions
                todo = filter (not . (`elem` done)) results

makeTransitions :: (Ord state, Alphabet input, Eq input) => Transition state input -> [input] -> S.Set state -> [(S.Set state, input, S.Set state)]
makeTransitions transition alphabet states = zipWith ((,,) states) alphabet results
  where results = map (transitionSet transition states) alphabet

transitionSet :: (Ord state, Alphabet input, Eq input) => Transition state input -> S.Set state -> input -> S.Set state
transitionSet transition states input = S.union newStates closure
  where newStates = S.unions . S.map S.fromList . S.map (`transition` input) $ states
        closure = S.unions . S.map (epsilonClosure transition) $ newStates
