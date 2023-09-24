module Automata.DFA (DFA (..), eval, normalize) where

import Data.Maybe
import qualified Data.Map as M

data DFA state input = DFA state [(state, input, state)] [state] [input] deriving (Show)

eval :: (Ord a, Ord b) => DFA a b -> [b] -> Bool
eval (DFA initial transitions accept _) word = foldr transFunc initial word `elem` accept
  where transFunc input state = fromJust (M.lookup (state, input) transMap)
        (states, inputs, results) = unzip3 transitions
        transMap = M.fromList (zip (zip states inputs) results)

normalize :: (Ord a) => DFA a b -> DFA Int b
normalize (DFA initial transitions accept alphabet) = DFA (replace initial) (zip3 indices inputs (map replace results)) (map replace accept) alphabet
  where indices = concatMap (replicate (length alphabet)) [0..]
        (states, inputs, results) = unzip3 transitions
        dict = M.fromList (zip states indices)
        replace x = fromJust (M.lookup x dict)