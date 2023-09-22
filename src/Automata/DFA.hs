module Automata.DFA (DFA (..), eval) where

import Data.Maybe
import qualified Data.Map as M

data DFA state input = DFA state [(state, input, state)] [state]

eval :: (Ord a, Ord b) => DFA a b -> [b] -> Bool
eval (DFA initial transition accept) vals = foldr trans initial vals `elem` accept
  where trans input state = fromJust (M.lookup (state, input) valMap)
        (states, inputs, results) = unzip3 transition
        valMap = M.fromList (zip (zip states inputs) results)