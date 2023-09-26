module MinimizeSpec (spec) where

import Test.Hspec
import Automata.DFA

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

minimized :: DFA Int Char
minimized = minimize dfa

spec :: Spec
spec = do
  describe "Normal DFA" $ do
    it "It recognizes 'ab'" $ do
      eval dfa "ab" `shouldBe` True
  describe "Minimized DFA" $ do
    it "It recognizes 'ab'" $ do
      eval minimized "ab" `shouldBe` True