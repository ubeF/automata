module Regular.NFA.Operators (singleton, emptyWord, emptyLang, kleeneStar, logicalOr, concatenate, normalize) where


import Regular.NFA (NFA(..))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust)


data Marker a = A a | B a | New a deriving (Ord, Eq, Show)


transformStates :: (a -> b) -> NFA a c -> NFA b c
transformStates f nfa = nfa { 
      states=newStates
    , initial=newInitial
    , accept=newAccept
    , transitions=newTransitions 
    }
  where newInitial = f . initial $ nfa
        newStates = map f. states $ nfa
        newAccept = map f . accept $ nfa
        (a, b, c) = unzip3 . transitions $ nfa
        newTransitions = zip3 (map f a) b (map f c)

normalize :: (Ord a) => NFA a b -> NFA Int b
normalize nfa = transformStates func nfa
  where valMap = M.fromList $ zip (states nfa) [1..]
        func x = fromJust $ M.lookup x valMap

concatenate :: (Ord input) => NFA state input -> NFA state input -> NFA (Marker state) input
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
                         map (\x -> (x , Nothing, initial nfaB)) (accept nfaA)

logicalOr :: (Ord input) => NFA state input -> NFA state input -> NFA (Marker state) input
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
                         [(newInitial, Nothing, initial nfaA), (newInitial, Nothing, initial nfaB)] <>
                         map (\x -> (x , Nothing, newAccept)) (accept nfaA) <> 
                         map (\x -> (x , Nothing, newAccept)) (accept nfaB)

kleeneStar :: NFA state input -> NFA (Marker state) input
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
                         [(newInitial, Nothing, initial nfaA), (newInitial, Nothing, newAccept)] <>
                         map (\x -> (x , Nothing, newAccept)) (accept nfaA) <>
                         map (\x -> (x , Nothing, initial nfaA)) (accept nfaA)

emptyLang :: NFA Int input
emptyLang = NFA {
    states = [0, 1]
  , alphabet = []
  , transitions = []
  , initial = 0
  , accept = [1]
  }

emptyWord :: NFA Int input
emptyWord = NFA {
    states = [0, 1]
  , alphabet = []
  , transitions = [(0, Nothing, 1)]
  , initial = 0
  , accept = [1]
  }

singleton :: input -> NFA Int input
singleton x = NFA {
    states = [0, 1]
  , alphabet = [x]
  , transitions = [(0, Just x, 1)]
  , initial = 0
  , accept = [1]
  }



