data State = Q0 | Q1 | Q2 | Q3 deriving (Eq)

transition :: State -> Char -> [State]
transition _ 'e' = []
transition Q0 'a' = [Q0, Q1]
transition Q0 'b' = [Q0]
transition Q1 'a' = []
transition Q1 'b' = [Q2]
transition Q2 'a' = [Q3]
transition Q2 'b' = []
transition Q3 'a' = [Q3]
transition Q3 'b' = [Q3]

testNFA :: NFA State Char
testNFA = NFA Q0 transition (==Q3) 'e'