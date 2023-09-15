module Automata.NFA (NFA, eval) where 

data NFA state input = NFA state (state -> input -> [state]) (state -> Bool) input

eval :: NFA a b -> [b] -> Bool
eval (NFA initial transition accept epsilon) vals = run [(initial, vals)]
  where run vals = case evaluateConfigs accept vals of
                        Success -> True
                        Failure -> False
                        Continue -> run . concatMap (genConfigs transition epsilon) $ vals

data Result = Success | Failure | Continue deriving (Show)

generateConfigs :: (state -> input -> [state]) -> state -> input -> [input] -> [(state, [input])]
generateConfigs transition state input rest = zip (transition state input) (repeat rest)

genConfigs :: (state -> input -> [state]) -> input -> (state, [input]) -> [(state, [input])]
genConfigs transition epsilon config = configs <> epsilonConfigs
  where configs 
          | null . snd $ config = []
          | otherwise = generateConfigs transition (fst config) (head . snd $ config) (tail . snd $ config)
        epsilonConfigs = generateConfigs transition (fst config) epsilon (snd config)

evaluateConfigs :: (state -> Bool) -> [(state, [input])] -> Result
evaluateConfigs _ [] = Failure
evaluateConfigs accept configs
  | any (accept . fst) . filter (null . snd) $ configs = Success
  | otherwise = Continue


-- evalNFA :: NFA a b -> [b] -> Bool
-- evalNFA (NFA initial transition accept epsilon) vals = run (initial, vals)
--   where generate = generateConfigs transition
--         run (state, []) = accept state || any run (generate state epsilon [])
--         run (state, x:xs) = any run (generate state x xs <> generate state epsilon (x:xs))




