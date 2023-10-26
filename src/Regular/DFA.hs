module Regular.DFA (DFA (..), eval, normalize, minimize, getTransitionFunction, getAcceptFunction, run, findJunkState, step, findNecessarilyDistinctPairs, findNonDistinctPairs) where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Debug.Trace

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
        (stateInputs, inputs, results) = unzip3 $ transitions dfa
        transMap = trace "created Map" $ M.fromList (zip (zip stateInputs inputs) results)

getAcceptFunction :: (Eq a) => DFA a b -> (a -> Bool)
getAcceptFunction dfa = (`elem` accept dfa)

findJunkState :: (Ord a, Ord b) => DFA a b -> Maybe a
findJunkState dfa = case filter (\(a, b) -> all (==a) b) resultPairs of
    [] -> Nothing
    (x:_) -> Just . fst $ x
  where f = getTransitionFunction dfa
        resultPairs = zip (states dfa) $ map (\x -> map (f x) (alphabet dfa)) (states dfa)

eval :: (Ord a, Ord b) => DFA a b -> [b] -> Bool
eval dfa xs = getAcceptFunction dfa . initial $ run dfa xs

run :: (Ord a, Ord b) => DFA a b -> [b] -> DFA a b
run dfa [] = dfa
run dfa (x:xs) = run (step dfa x) xs

step :: (Ord a, Ord b) => DFA a b -> b -> DFA a b
step dfa x = dfa {initial=newState}
    where newState = (getTransitionFunction dfa) (initial dfa) x

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

-- findNonDistinctPairs :: (Ord a, Ord b) => DFA a b -> (S.Set (S.Set a), S.Set (S.Set a)) -> S.Set (S.Set a)
-- findNonDistinctPairs dfa (potential, distinct)
--   | S.null newDistinct = potential
--   | otherwise = findNonDistinctPairs dfa (S.difference potential newDistinct, S.union distinct newDistinct)
--   where newDistinct = S.filter (any (`elem` distinct) . bulkTransitionSet (alphabet dfa)) potential
--         transition = flip . getTransitionFunction $ dfa
--         transitionSet set input = S.map (transition input) set
--         bulkTransitionSet inputs set = map (transitionSet set) inputs


findNonDistinctPairs :: (Ord a, Ord b) => DFA a b -> (S.Set (S.Set a), S.Set (S.Set a)) -> S.Set (S.Set a)
findNonDistinctPairs dfa (potential, distinct) = go potentiated distinct
  where f = flip . getTransitionFunction $ dfa
        potentiated = undefined
        go a b
            | S.null newDistinct = b
            | otherwise = go (S.difference potential newDistinct, S.union distinct newDistinct)
            where newDistinct = S.filter (any (`elem` distinct) . bulkTransitionSet (alphabet dfa)) potential


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

instance (Show a, Show b, Ord a, Ord b) => Show (DFA a b) where
  show dfa = L.intercalate "\n" $ header : rows
    where inputWidth = maximum . map length . map show . alphabet $ dfa
          stateWidth = maximum . map length . map show . states $ dfa
          header = L.intercalate " | " [
              "   "
            , padRight stateWidth ""
            , (unwords . map (padRight inputWidth) . map show . alphabet $ dfa)
            ]
          rows = map makeRow . states $ dfa
          makeRow x = L.intercalate " | " [
              mconcat [if getAcceptFunction dfa x then "*" else " ", " ", if x == initial dfa then ">" else " "]
            , padRight stateWidth . show $ x
            , unwords . map (padRight inputWidth) . map show $ map (getTransitionFunction dfa x) (alphabet dfa)
            ]

padRight :: Int -> String -> String
padRight len str = max str padded
  where padded = take len $ str <> repeat ' '
