module Automate.DFA (DFA, eval) where 

data DFA state input = DFA state (state -> input -> state) (state -> Bool)

eval :: DFA a b -> [b] -> Bool
eval (DFA init trans acc) input = acc . foldr (flip trans) init $ input





