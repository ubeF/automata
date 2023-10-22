module Tokenizer where

import Tokenizer.DFA
import Data.Maybe

type Rule a b = (DFA Int a, [a] -> b)
type Tokenizer a b = [Rule a b]

apply :: Rule a b -> [a] -> b
apply = snd

isMatched :: Rule a b -> Bool
isMatched (dfa, _) = isAccepting dfa

advance :: Rule a b -> a -> Rule a b
advance (dfa, f) x = (step dfa x, f)

tokenize :: (Ord a) => Tokenizer a b -> [a] -> [b]
tokenize _ [] = []
tokenize tok input = token : tokenize tok rest
  where (token, rest) = fromJust $ scan tok Nothing [] input
        scan _ match _ [] = match
        scan tokenizer match prev (x:xs) 
          | all (isJunked . fst) tokenizer = match
          | otherwise = case filter isMatched advanced of
              [] -> scan advanced match (x:prev) xs
              (top:_) -> scan advanced (Just (apply top . reverse $ x:prev, xs)) (x:prev) xs
              where advanced = map (`advance` x) tokenizer
          