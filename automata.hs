data DFA state input = DFA state (state -> input -> state) (state -> Bool)

data NFA state input = NFA state (state -> input -> [state]) (state -> Bool)

evalDFA :: DFA a b -> [b] -> Bool
evalDFA (DFA init trans acc) = acc . foldr (flip trans) init

evalNFA :: NFA a b -> [b] -> Bool
evalNFA (NFA state transition accept) [] = accept state
evalNFA (NFA state transition accept) (x:xs) = any recurse newStates
  where newStates = transition state x
        recurse newState = evalNFA (NFA newState transition accept) xs


data State = Odd | Even deriving (Show, Eq)
data Input = Zero | One deriving (Show)

transFunc :: State -> Input -> State
transFunc Odd Zero = Odd
transFunc Odd One = Even
transFunc Even Zero = Even
transFunc Even One = Odd

testDFA = DFA Even transFunc (Even ==)

test = evalDFA testDFA
