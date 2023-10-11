module ExampleSpec (spec) where

import Test.Hspec
import Automata.NFA
import qualified Automata.DFA as DFA

testTransition :: [(Int, Char, Int)]
testTransition = [
    (0, epsilon, 1)
  , (0, epsilon, 2)
  , (1, 'b', 3)
  , (2, 'a', 4)
  , (3, 'a', 5)
  , (4, 'b', 6)
  , (5, epsilon, 7)
  , (6, epsilon, 7)
  , (7, epsilon, 0)
  ]

-- testNFA :: NFA Int Char
-- testNFA = NFA 0 testTransition [7] "ab"

testNFA :: NFA Int Char
testNFA = NFA [0..7] "ab" testTransition 0 [7]

testDFA :: DFA.DFA Int Char
testDFA = toDFA testNFA

spec :: Spec
spec = do 
  describe "NFA matching the regex (ab+ba)(ab+ba)*" $ do
    it "Recognizes 'ab'" $ do
      eval testNFA "ab" `shouldBe` True
    it "Doesn't recognize ''" $ do
      eval testNFA "" `shouldBe` False
    it "Recognizes 'abba'" $ do
      eval testNFA "abba" `shouldBe` True
  describe "DFA acquired via conversion from NFA" $ do
    it "Recognizes 'ab'" $ do
      DFA.eval testDFA "ab" `shouldBe` True
    it "Doesn't recognize ''" $ do
      DFA.eval testDFA "" `shouldBe` False
    it "Recognizes 'abba'" $ do
      DFA.eval testDFA "abba" `shouldBe` True