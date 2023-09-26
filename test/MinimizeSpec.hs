module MinimizeSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Automata.DFA

newtype Word = Word String deriving (Show)

cons :: Char -> MinimizeSpec.Word -> MinimizeSpec.Word
cons x (Word xs) = Word (x:xs)

instance Arbitrary MinimizeSpec.Word where
  arbitrary = sized $ \n ->
    frequency
      [ (1, return (MinimizeSpec.Word []))
      , (n, cons <$> elements "ab" <*> arbitrary)
      ]

prop_sameResult :: DFA Int Char -> MinimizeSpec.Word -> Bool
prop_sameResult dfa (Word word) = eval dfa word == eval (minimize dfa) word

transitions :: [(Int, Char, Int)]
transitions = [
    (0, 'a', 1)
  , (0, 'b', 2)
  , (1, 'a', 3)
  , (1, 'b', 3)
  , (3, 'a', 1)
  , (3, 'b', 1)
  , (2, 'a', 4)
  , (2, 'b', 4)
  , (4, 'a', 2)
  , (4, 'b', 2)
  ]

dfa :: DFA Int Char
dfa = DFA 0 transitions [3,4] "ab"

spec :: Spec
spec = do
  describe "Minimization of DFAs" $ do
    it "Recognizes the same words" $ 
      property (prop_sameResult dfa) 