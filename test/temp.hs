import Regular
import qualified Regular.NFA as N
import qualified Regular.DFA as D

digits = times (1,4) (range ['0'..'9'])
ip = digits <> lit '.' <> digits <> lit '.' <> digits <> lit '.' <> digits

regex = lit 'a' <> star (lit 'b') <> lit 'a'

dfa = N.toDFA ip

-- unoptimized (63.06 secs, 110,196,111,720 bytes)