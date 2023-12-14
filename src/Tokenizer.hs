module Tokenizer (Tokenizer, Rule, makeRule, makeDefault, tokenize) where


import Tokenizer.DFA
import Regular.NFA (NFA)
import Data.Maybe
import qualified Data.Set as S


type Rule a b = (DFA Int a, [a] -> b)
type Tokenizer a b = [Rule a b] 


apply :: Rule a b -> [a] -> b
apply = snd

makeRule :: (Ord a, Ord b) => NFA a b -> ([b] -> c) -> Rule b c
makeRule nfa f = (compile nfa, f)

isMatched :: Rule a b -> Bool
isMatched (dfa, _) = isAccepting dfa

advance :: Rule a b -> a -> Rule a b
advance (dfa, f) x = (step dfa x, f)

matchOne :: DFA Int b
matchOne = DFA {
    transition = func
  , current = 0
  , accept = S.singleton 1
  , junk = 2
  }
  where func 0 _ = 1
        func 1 _ = 2
        func 2 _ = 2
        func _ _ = 2

makeDefault :: ([a] -> b) -> Rule a b
makeDefault f = (matchOne, f)

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
          
