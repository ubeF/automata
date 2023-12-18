module Regex where


import Tokenizer
import Regular


data Token = RightParanthesis
           | LeftParanthesis
           | Or
           | KleeneStar
           | Literal String


tokenizer :: Tokenizer Char Token
tokenizer = [
    makeRule (lit ')') (const RightParanthesis)
  , makeRule (lit '(') (const LeftParanthesis)
  , makeRule (lit '*') (const KleeneStar)
  , makeRule (lit '+') (const Or)
  , makeRule (lit '\\' <> range "()+*") (Literal . tail)
  , makeDefault Literal
  ]
