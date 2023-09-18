module Automata.NFA (NFA (..), Alphabet (..), eval, makeCharNFA, makeTransition) where 

import Data.Maybe
import qualified Data.Map as M

class Alphabet a where
  epsilon :: a

instance Alphabet Char where
  epsilon = toEnum 0

data NFA state input = NFA state (state -> input -> [state]) (state -> Bool)

data Result = Success | Failure | Continue deriving (Show)

data Config state input = Config state [input]

eval :: Alphabet b => NFA a b -> [b] -> Bool
eval (NFA initial transition accept) vals = run [Config initial vals]
  where run vals = case evaluateConfigs accept vals of
                        Success -> True
                        Failure -> False
                        Continue -> run . concatMap (generateConfigs transition) $ vals

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

makeCharNFA :: (Ord state) => state -> [((state, Char), [state])] -> [state] -> NFA state Char
makeCharNFA initial valMap acceptStates = NFA initial (makeTransition valMap) (`elem` acceptStates) 

makeTransition :: (Ord a, Ord b) => [((b, a), [b])] -> (b -> a -> [b])
makeTransition triples = transition
  where stateMap = M.fromList triples
        transition state input = case M.lookup (state, input) stateMap of
          (Just newState) -> newState
          Nothing -> []