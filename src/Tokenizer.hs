module Tokenizer where

import Automata.DFA
import Automata.NFA (toDFA)
import Automata

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
              (top:_) -> scan advanced (Just (apply top . reverse $ x:prev, xs)) (x:prev) xs
              where advanced = map (`advance` x) tokenizer
          
-- Test

data IP  = IP String | Junk deriving Show

digits :: NFA Int Char
digits = times (1,4) (range ['0'..'9'])

ip :: DFA Int Char
ip = minimize . toDFA . makeAscii $ digits <> lit '.' <> digits <> lit '.' <> digits <> lit '.' <> digits

ipRule :: Rule Char IP
ipRule = (ip, IP)

junk :: DFA Int Char
junk = minimize . toDFA . makeAscii $ range ascii

junkRule :: Rule Char IP
junkRule = (junk, const Junk)

ipTokenizer :: Tokenizer Char IP
ipTokenizer = [ipRule, junkRule]

testString :: String
testString = "ad f123.456.3.1     fdsf!d 1.2.3.4ffdf"

result :: [IP]
result = tokenize ipTokenizer testString