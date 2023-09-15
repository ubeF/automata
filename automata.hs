{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use map" #-}

data DFA state input = DFA state (state -> input -> state) (state -> Bool)

data NFA state input = NFA state (state -> input -> [state]) (state -> Bool) input

evalDFA :: DFA a b -> [b] -> Bool
evalDFA (DFA init trans acc) input = acc . foldr (flip trans) init $ input

generateConfigs :: (state -> input -> [state]) -> state -> input -> [input] -> [(state, [input])]
generateConfigs transition state input rest = zip (transition state input) (repeat rest)

evalNFA :: NFA a b -> [b] -> Bool
evalNFA (NFA initial transition accept epsilon) vals = run (initial, vals)
  where generate = generateConfigs transition
        run (state, []) = accept state || any run (generate state epsilon [])
        run (state, x:xs) = any run (generate state x xs <> generate state epsilon (x:xs))
        
data State = Odd | Even deriving (Show, Eq)
data Input = Zero | One deriving (Show)

transFunc :: State -> Input -> State
transFunc Odd Zero = Odd
transFunc Odd One = Even
transFunc Even Zero = Even
transFunc Even One = Odd

testDFA = DFA Even transFunc (Even ==)

test = evalDFA testDFA
