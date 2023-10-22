module TokenTest where
import Tokenizer
import Automata

digits :: NFA Int Char
digits = times (1,4) (range ['0'..'9'])

data IP = IP String | Junk deriving (Show)

ipTokenizer :: Tokenizer Char IP
ipTokenizer = [
    makeRule (digits <> lit '.' <> digits <> lit '.' <> digits <> lit '.' <> digits) IP
  , makeDefault (const Junk)
  ]

testString = "dfdasd 1.2.3.4 dasd dsad    123.44.5.6 ff df"