import Automata.NFA

data State = Q0 | Q1 | Q2 | Q3 deriving (Eq)

transitionFunc :: State -> Char -> [State]
transitionFunc _ 'e' = []
transitionFunc Q0 'a' = [Q0]
transitionFunc Q0 'b' = [Q0, Q1]
transitionFunc Q1 'a' = [Q2]
transitionFunc Q1 'b' = [Q2]
transitionFunc Q2 'a' = [Q3]
transitionFunc Q2 'b' = [Q3]
transitionFunc Q3 _ = []

testNFA :: NFA State Char
testNFA = NFA Q0 transitionFunc (==Q3) 'e'