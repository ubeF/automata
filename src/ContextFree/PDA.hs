module ContextFree.PDA where


import qualified Data.Map as M
import Data.Maybe


type PDATransition stateType alphabetType stackAlphabetType = (stateType, Maybe alphabetType, stackAlphabetType, [stackAlphabetType], stateType)

data PDA stateType alphabetType stackAlphabetType = PDA {
    state :: stateType
  , stack :: [stackAlphabetType]
  , transitions :: [PDATransition stateType alphabetType stackAlphabetType]
}


makeKeyValue :: PDATransition stateType alphabetType stackAlphabetType -> ((stateType, Maybe alphabetType, stackAlphabetType), [([stackAlphabetType], stateType)])
makeKeyValue (a, b, c, d, e) = ((a, b, c), [(d, e)])

getTransitionFunction ::
  (Ord stateType, Ord alphabetType, Ord stackAlphabetType) =>
  PDA stateType alphabetType stackAlphabetType ->
  (stateType -> Maybe alphabetType -> stackAlphabetType -> [([stackAlphabetType], stateType)])
getTransitionFunction pda = func
  where valMap = foldr (\(k, v) m -> M.insertWith (<>) k v m) M.empty pairs
        pairs = map makeKeyValue . transitions $ pda
        func a b c = fromMaybe [] (M.lookup (a, b, c) valMap)


step :: (Ord stateType, Ord alphabetType, Ord stackAlphabetType) => PDA stateType alphabetType stackAlphabetType -> Maybe alphabetType -> [PDA stateType alphabetType stackAlphabetType]
step pda input
  | null . stack $ pda = []
  | otherwise = do
    (newStack, newState) <- f (state pda) input (head . stack $ pda)
    return pda {
        state = newState
      , stack = newStack <> (tail . stack $ pda)
      }
      where f = getTransitionFunction pda

stackEmpty :: PDA stateType alphabetType stackAlphabetType -> Bool
stackEmpty = null . stack


type Config stateType alphabetType stackAlphabetType = (PDA stateType alphabetType stackAlphabetType, [alphabetType])

isAccepting :: Config stateType alphabetType stackAlphabetType -> Bool
isAccepting (dfa, inputs) = stackEmpty dfa && null inputs

readNext :: (Ord stateType, Ord alphabetType, Ord stackAlphabetType) => Config stateType alphabetType stackAlphabetType -> [Config stateType alphabetType stackAlphabetType]
readNext (_, []) = []
readNext (pda, x : xs) = map (\a -> (a, xs)) . step pda . Just $  x

readEpsilon :: (Ord stateType, Ord alphabetType, Ord stackAlphabetType) => Config stateType alphabetType stackAlphabetType -> [Config stateType alphabetType stackAlphabetType]
readEpsilon (pda, xs) = map (\a -> (a, xs)) . step pda $ Nothing

advanceConfig :: (Ord stateType, Ord alphabetType, Ord stackAlphabetType) => Config stateType alphabetType stackAlphabetType -> [Config stateType alphabetType stackAlphabetType]
advanceConfig x = readNext x <> readEpsilon x

eval :: (Ord stateType, Ord alphabetType, Ord stackAlphabetType) => PDA stateType alphabetType stackAlphabetType -> [alphabetType] -> Bool
eval pda xs = run [(pda, xs)]
  where run [] = False
        run configs = any isAccepting configs || (run . concatMap advanceConfig $ configs)