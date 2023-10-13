module Automata.Operators (singleton, emptyWord, emptyLang, kleeneStar, logicalOr, concatenate) where

import Automata.NFA
import qualified Data.Set as S

data Marker a = A a | B a | New a deriving (Ord, Eq, Show)

concatenate :: (Alphabet input, Ord input) => NFA state input -> NFA state input -> NFA (Marker state) input
concatenate nfa1 nfa2 = NFA {
    states = newStates
  , alphabet = newAlphabet
  , transitions = newTransitions  
  , initial = initial nfaA
  , accept = accept nfaB
  }
  where nfaA = transformStates A nfa1
        nfaB = transformStates B nfa2
        newAlphabet = S.toList $ S.union (S.fromList . alphabet $ nfaA) (S.fromList . alphabet $ nfaB)
        newStates = states nfaA <> states nfaB
        newTransitions = transitions nfaA <> 
                         transitions nfaB <> 
                         map (\x -> (x , epsilon, initial nfaB)) (accept nfaA)

logicalOr :: (Alphabet input, Ord input) => NFA state input -> NFA state input -> NFA (Marker state) input
logicalOr nfa1 nfa2 = NFA {
    states = newStates
  , alphabet = newAlphabet
  , transitions = newTransitions 
  , initial = newInitial
  , accept = [newAccept]
  }
  where nfaA = transformStates A nfa1
        nfaB = transformStates B nfa2
        newAlphabet = S.toList $ S.union (S.fromList . alphabet $ nfaA) (S.fromList . alphabet $ nfaB)
        newInitial = New . initial $ nfa1
        newAccept = New . head . accept $ nfa1
        newStates = states nfaA <> states nfaB <> [newInitial, newAccept]
        newTransitions = transitions nfaA <> 
                         transitions nfaB <>
                         [(newInitial, epsilon, initial nfaA), (newInitial, epsilon, initial nfaB)] <>
                         map (\x -> (x , epsilon, newAccept)) (accept nfaA) <> 
                         map (\x -> (x , epsilon, newAccept)) (accept nfaB)

kleeneStar :: (Alphabet input) => NFA state input -> NFA (Marker state) input
kleeneStar nfa = NFA {
    states = newStates
  , alphabet = alphabet nfaA
  , transitions = newTransitions 
  , initial = newInitial
  , accept = [newAccept]
  }
  where nfaA = transformStates A nfa
        newInitial = New . initial $ nfa
        newAccept = New . head . accept $ nfa
        newStates = states nfaA <> [newAccept, newInitial]
        newTransitions = transitions nfaA <> 
                         [(newInitial, epsilon, initial nfaA), (newInitial, epsilon, newAccept)] <>
                         map (\x -> (x , epsilon, newAccept)) (accept nfaA) <>
                         map (\x -> (x , epsilon, initial nfaA)) (accept nfaA)

emptyLang :: NFA Int input
emptyLang = NFA {
    states = [0, 1]
  , alphabet = []
  , transitions = []
  , initial = 0
  , accept = [1]
  }

emptyWord :: (Alphabet input) => NFA Int input
emptyWord = NFA {
    states = [0, 1]
  , alphabet = []
  , transitions = [(0, epsilon, 1)]
  , initial = 0
  , accept = [1]
  }

singleton :: input -> NFA Int input
singleton x = NFA {
    states = [0, 1]
  , alphabet = [x]
  , transitions = [(0, x, 1)]
  , initial = 0
  , accept = [1]
  }



