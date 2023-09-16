module Automata.DFA (DFA, eval) where 

data DFA state input = DFA state (state -> input -> state) (state -> Bool)

eval :: DFA a b -> [b] -> Bool
eval (DFA initial trans acc) = acc . foldr (flip trans) initial





