module Automata.NFA (NFA (..), eval) where 

class Alphabet a where
  epsilon :: a

data NFA state input = NFA state (state -> input -> [state]) (state -> Bool)

data Result = Success | Failure | Continue deriving (Show)

data Config state input = Config state [input]

eval :: Alphabet b => NFA a b -> [b] -> Bool
eval (NFA initial transition accept) vals = run [Config initial vals]
  where run vals = case evaluateConfigs accept vals of
                        Success -> True
                        Failure -> False
                        Continue -> run . concatMap (generateConfigs transition) $ vals

generateConfigs :: Alphabet a => (b -> a -> [b]) -> Config b a -> [Config b a]
generateConfigs transition (Config state word) = configs <> epsilonConfigs
  where gen input rest = zipWith Config (transition state input) (repeat rest)
        configs 
          | null word = []
          | otherwise = gen (head word) (tail word)
        epsilonConfigs = gen epsilon word

evaluateConfig :: (a -> Bool) -> Config a b -> Bool
evaluateConfig accept (Config state word) 
  | null word = accept state
  | otherwise = False 

evaluateConfigs :: (a -> Bool) -> [Config a b] -> Result
evaluateConfigs _ [] = Failure
evaluateConfigs accept configs
  | any (evaluateConfig accept) configs = Success
  | otherwise = Continue

-- evalNFA :: NFA a b -> [b] -> Bool
-- evalNFA (NFA initial transition accept epsilon) vals = run (initial, vals)
--   where generate = generateConfigs transition
--         run (state, []) = accept state || any run (generate state epsilon [])
--         run (state, x:xs) = any run (generate state x xs <> generate state epsilon (x:xs))




