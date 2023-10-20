module Tokenizer where

import Automata.DFA

type Rule a b = (DFA Int a, [a] -> b)
type Tokenizer a b = [Rule a b]

isMatched :: Rule a b -> Bool
isMatched (dfa, _) = getAcceptFunction dfa (initial dfa)

advance :: (Ord a) => Rule a b -> a -> Rule a b
advance (dfa, f) x = (step dfa x, f) 

apply :: Rule a b -> [a] -> b
apply = snd

tokenize :: (Ord a) => Tokenizer a b -> [a] -> [b]
tokenize _ [] = []
tokenize tok input = token : tokenize tok rest
  where Just (token, rest) = scan tok Nothing [] input
        scan _ match _ [] = match
        scan tokenizer match prev (x:xs) 
          | all (isStuck . fst) tokenizer = match
          | otherwise = case filter isMatched advanced of
              [] -> scan advanced match (x:prev) xs
              (top:_) -> scan advanced (Just (apply top . reverse $ prev, xs)) (x:prev) xs
              where advanced = map (`advance` x) tokenizer
          
                                        