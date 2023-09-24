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

testNFA = NFA 0 testTransition [7]