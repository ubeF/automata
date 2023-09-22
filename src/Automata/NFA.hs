module Automata.NFA (NFA (..), Alphabet (..), toDFA) where

import qualified Data.Set as S
import qualified Automata.DFA as DFA
import qualified Data.Map as M
import Data.Maybe

class Alphabet a where
  epsilon :: a

instance Alphabet Char where
  epsilon = toEnum 0

data NFA state input = NFA state [(state, input, state)] [state]

data Result = Success | Failure | Continue deriving (Show)

data Config state input = Config state [input]

eval :: (Ord a, Ord b, Alphabet b) => NFA a b -> [b] -> Bool
eval (NFA initial transitions accept) vals = run [Config initial vals]
  where run vals = case evaluateConfigs (`elem` accept) vals of
                        Success -> True
                        Failure -> False
                        Continue -> run . concatMap (generateConfigs transFunc) $ vals
                          where transFunc = makeFunction (zip3 states inputs resultLists)
                                (states, inputs, _) = unzip3 transitions
                                resultLists = zipWith (\a b -> map (\(_, _, z) -> z) . filter (\(x, y, _) -> a==x && b==y) $ transitions) states inputs

generateConfigs :: Alphabet a => (b -> a -> [b]) -> Config b a -> [Config b a]
generateConfigs transition (Config state word) = configs <> epsilonConfigs
  where gen input rest = zipWith Config (transition state input) (repeat rest)
        configs
          | null word = []
          | otherwise = gen (head word) (tail word)
        epsilonConfigs = gen epsilon word

evaluateConfig :: (a -> Bool) -> Config a b -> Bool
evaluateConfig accept (Config state word)
  | null word = accept state
  | otherwise = False

evaluateConfigs :: (a -> Bool) -> [Config a b] -> Result
evaluateConfigs _ [] = Failure
evaluateConfigs accept configs
  | any (evaluateConfig accept) configs = Success
  | otherwise = Continue

makeFunction :: (Ord a, Ord b) => [(a, b, [a])] -> (a -> b -> [a])
makeFunction tuples = function
  where funcMap = M.fromList ( zip (zip keys1 keys2) vals)
        (keys1, keys2, vals) = unzip3 tuples
        function a b = case M.lookup (a,b) funcMap of
          Nothing -> []
          (Just val) -> val

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

toDFA :: (Ord a, Alphabet b, Eq b) => NFA a b -> [b] -> DFA.DFA Int b
toDFA (NFA initial transitions accept) alphabet = DFA.normalize (DFA.DFA newInitial newTransitions newAccept) alphabet
  where newInitial = epsilonClosure transitions initial
        newTransitions = potentiateStates transitions alphabet newInitial
        (newStates, _, _) = unzip3 newTransitions
        newAccept = filter (any (`elem` accept)) . S.toList . S.fromList $ newStates


-- Tests


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

testNFA = NFA 0 testTransition [7]