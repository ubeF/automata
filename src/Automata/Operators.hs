module Automata.Operators (singleton, emptyWord, emptyLang, kleene, _kleene, Automata.Operators.or, Automata.Operators.concat) where

import Automata.NFA
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

data Marker a = A a | B a | New a deriving (Ord, Eq, Show)

transformStates :: (a -> b) -> NFA a input -> NFA b input
transformStates f (NFA initial transitions accept alphabet) = NFA (f initial) newTransitions (map f accept) alphabet
  where (a, b, c) = unzip3 transitions
        newTransitions = zip3 (map f a) b (map f c)

normalize :: (Ord a) => NFA a input -> NFA Int input
normalize nfa = transformStates func nfa
  where (NFA initial transitions accept _) = nfa
        (states, _, results) = unzip3 transitions
        uniqStates = S.toList $ S.insert initial $ S.union (S.fromList accept) $ S.union (S.fromList states) (S.fromList results)
        valMap = M.fromList $ zip uniqStates [1..]
        func x = fromJust $ M.lookup x valMap

concat :: (Ord state, Alphabet input, Ord input) => NFA state input -> NFA state input -> NFA Int input
concat nfa1 nfa2 = normalize $ _concat nfa1 nfa2

or :: (Ord state, Alphabet input, Ord input) => NFA state input -> NFA state input -> NFA Int input
or nfa1 nfa2 = normalize $ Automata.Operators._or nfa1 nfa2

kleene :: (Ord state, Alphabet input, Ord input) => NFA state input -> NFA Int input
kleene = normalize . _kleene

_concat :: (Alphabet input, Ord input) => NFA state input -> NFA state input -> NFA (Marker state) input
_concat nfa1 nfa2 = NFA initialA newTransitions acceptB newAlphabet
  where (NFA initialA transitionsA acceptA alphabetA) = transformStates A nfa1
        (NFA initialB transitionsB acceptB alphabetB) = transformStates B nfa2
        newAlphabet = S.toList $ S.union (S.fromList alphabetA) (S.fromList alphabetB)
        newTransitions = transitionsA <> 
                         transitionsB <> 
                         map (\x -> (x , epsilon, initialB)) acceptA

_or :: (Alphabet input, Ord input) => NFA state input -> NFA state input -> NFA (Marker state) input
_or nfa1 nfa2 = NFA newInitial newTransitions [newAccept] newAlphabet
  where (NFA initialA transitionsA acceptA alphabetA) = transformStates A nfa1
        (NFA initialB transitionsB acceptB alphabetB) = transformStates B nfa2
        newAlphabet = S.toList $ S.union (S.fromList alphabetA) (S.fromList alphabetB)
        newInitial = New . getInitial $ nfa1
        newAccept = New . head . getAcceptStates $ nfa1
        newTransitions = transitionsA <> 
                         transitionsB <> 
                         [(newInitial, epsilon, initialA), (newInitial, epsilon, initialB)] <>
                         map (\x -> (x , epsilon, newAccept)) acceptA <>
                         map (\x -> (x , epsilon, newAccept)) acceptB

_kleene :: (Alphabet input) => NFA state input -> NFA (Marker state) input
_kleene nfa = NFA newInitial newTransitions [newAccept] alphabet
  where (NFA initial transitions accept alphabet) = transformStates A nfa
        newInitial = New . getInitial $ nfa
        newAccept = New . head . getAcceptStates $ nfa
        newTransitions = transitions <> 
                         [(newInitial, epsilon, initial), (newInitial, epsilon, newAccept)] <> 
                         map (\x -> (x , epsilon, newAccept)) accept <>
                         map (\x -> (x , epsilon, initial)) accept

emptyLang :: NFA Int input
emptyLang = NFA 0 [] [1] []

emptyWord :: (Alphabet input) => NFA Int input
emptyWord = NFA 0 [(0, epsilon, 1)] [1] [] 

singleton :: input -> NFA Int input
singleton x = NFA 0 [(0, x, 1)] [1] [x]



