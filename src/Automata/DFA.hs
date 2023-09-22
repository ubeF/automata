module Automata.DFA (DFA (..), eval, normalize) where

import Data.Maybe
import qualified Data.Map as M

data DFA state input = DFA state [(state, input, state)] [state] deriving (Show)

eval :: (Ord a, Ord b) => DFA a b -> [b] -> Bool
eval (DFA initial transitions accept) word = foldr transFunc initial word `elem` accept
  where transFunc input state = fromJust (M.lookup (state, input) transMap)
        (states, inputs, results) = unzip3 transitions
        transMap = M.fromList (zip (zip states inputs) results)

normalize :: (Ord a) => DFA a b -> [b] -> DFA Int b
normalize (DFA initial transitions accept) nalphabet = DFA (replace initial) (zip3 indices alphabet (map replace vals)) (map replace accept)
  where indices = concatMap (replicate (length nalphabet)) [0..]
        (keys, alphabet, vals) = unzip3 transitions
        dict = M.fromList (zip keys indices)
        replace x = fromJust (M.lookup x dict)