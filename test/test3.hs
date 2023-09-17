import Automata.DFA

data State = Q0 | Q1 | Q2 | Q3 | Q4 deriving (Eq, Show)

transition :: State -> Int -> State
transition Q0 0 = Q3
transition Q0 1 = Q1
transition Q1 0 = Q4
transition Q1 1 = Q2
transition Q2 0 = Q2
transition Q2 1 = Q2
transition Q3 0 = Q0
transition Q3 1 = Q4
transition Q4 0 = Q1
transition Q4 1 = Q2
transition _ _ = Q2

test :: DFA State Int
test = DFA Q0 transition (== Q1)