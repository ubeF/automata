module Test2 where

import Automata.NFA

data MyState = Q0 | Q1 | Q2 | Q3 deriving (Eq, Ord, Show)

transition :: [((MyState, Char), [MyState])]
transition = [
        ((Q0, 'a'), [Q0, Q1])
    ,   ((Q0, 'b'), [Q0])
    ,   ((Q1, 'a'), [])
    ,   ((Q1, 'b'), [Q2])
    ,   ((Q2, 'a'), [Q3])
    ,   ((Q2, 'b'), [])
    ,   ((Q3, 'a'), [Q3])
    ,   ((Q3, 'b'), [Q3])
    ]

automaton :: NFA MyState Char
automaton = makeCharNFA Q0 transition [Q3]
