module Regular.NFA.Evaluation (eval) where


import Regular.NFA (NFA(..), getAcceptFunction, getTransitionFunction, Transition)


data Result = Success | Failure | Continue deriving (Show)

data Config state input = Config state [input]


eval :: (Ord state, Ord input) => NFA state input -> [input] -> Bool
eval nfa input = run [Config (initial nfa) input]
  where run vals = case evaluateConfigs (getAcceptFunction nfa) vals of
                        Success -> True
                        Failure -> False
                        Continue -> run . concatMap (generateConfigs . getTransitionFunction $ nfa) $ vals

generateConfigs :: Transition state input -> Config state input -> [Config state input]
generateConfigs transition (Config state word) = configs <> epsilonConfigs
  where gen input rest = map (`Config` rest) (transition state input)
        configs
          | null word = []
          | otherwise = gen (Just . head $ word) (tail word)
        epsilonConfigs = gen Nothing word

evaluateConfigs :: (state -> Bool) -> [Config state input] -> Result
evaluateConfigs _ [] = Failure
evaluateConfigs acc configs
  | any (evaluateConfig acc) configs = Success
  | otherwise = Continue

evaluateConfig :: (state -> Bool) -> Config state input -> Bool
evaluateConfig acc (Config state word)
  | null word = acc state
  | otherwise = False

