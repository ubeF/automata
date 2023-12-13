module Regular.Minimal where

data DFA a b = DFA {
    transition :: a -> b -> a
  , states :: [a]
  , initial :: a
  , accept :: [a]
  , alphabet :: [b]
}

findJunkState :: (Eq a) => DFA a b -> Maybe a
findJunkState dfa = case filter (\(a, b) -> all (==a) b) resultPairs of
    [] -> Nothing
    (x:_) -> Just . fst $ x
  where f = transition dfa
        resultPairs = zip (states dfa) $ map (\x -> map (f x) (alphabet dfa)) (states dfa)
