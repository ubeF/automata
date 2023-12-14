module Regular.DFA (DFA (..), eval, normalize, minimize) where


import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Regular.Minimal as RM


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
        transMap = M.fromList (zip (zip stateInputs inputs) results)

getAcceptFunction :: (Eq a) => DFA a b -> (a -> Bool)
getAcceptFunction dfa = (`elem` accept dfa)

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


minimize :: (Ord a, Ord b) => DFA a b -> RM.DFA Int b
minimize dfa = RM.DFA {
    RM.states = states tmp
  , RM.transition = getTransitionFunction tmp
  , RM.alphabet = alphabet tmp
  , RM.initial = initial tmp
  , RM.accept = accept tmp
  }
  where tmp = normalize . removeDuplicateStates . mergeStates dfa . findNonDistinctPairs dfa . findNecessarilyDistinctPairs $ dfa

findNecessarilyDistinctPairs :: (Ord a) => DFA a b -> (S.Set (S.Set a), S.Set (S.Set a))
findNecessarilyDistinctPairs dfa = (potentialPairs, distinctPairs)
  where allStates = S.fromList . states $ dfa
        acceptingStates = S.fromList . accept $ dfa
        nonAcceptingStates = S.difference allStates acceptingStates
        potentialPairs = S.union (cartesianProduct acceptingStates) (cartesianProduct nonAcceptingStates)
        distinctPairs = S.difference (cartesianProduct allStates) potentialPairs

lookAhead :: (Ord a, Ord b) => DFA a b  -> S.Set (S.Set a) -> S.Set (S.Set a, S.Set (S.Set a))
lookAhead dfa = S.map (\x -> (x, f x))
  where f set = S.fromList $ funcs <*> pure set
        transition = flip . getTransitionFunction $ dfa
        funcs = map (S.map . transition) . alphabet $ dfa

findNonDistinctPairs :: (Ord a, Ord b) => DFA a b -> (S.Set (S.Set a), S.Set (S.Set a)) -> S.Set (S.Set a)
findNonDistinctPairs dfa (unknown, necessarilyDistinct) = go (lookAhead dfa unknown) necessarilyDistinct
  where go potential distinct 
          | S.null newDistinct = S.map fst potential
          | otherwise = go (S.difference potential newDistinct) (S.union distinct (S.map fst potential))
          where newDistinct = S.filter (not . S.disjoint distinct . snd) potential

mergeStates :: (Ord a) => DFA a b -> S.Set (S.Set a) -> DFA (S.Set a) b
mergeStates dfa pairs = transformStates f dfa
  where f x = foldr (\merge state -> if S.disjoint merge state then state else S.union merge state) (S.singleton x) pairs

cartesianProduct :: (Ord a) => S.Set a -> S.Set (S.Set a)
cartesianProduct set = S.filter ((>1) . S.size) . S.map tupleToSet $ S.cartesianProduct set set
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
