module Tokenizer
  ( Token(..)
  , offToken
  , Tokenizer.isDigit
  , Tokenizer.isLetter
  , Tokenizer.isSpace
  , tokenizer
  , tokenString
  , destoken
  , destokenize
  ) where

import Data.Char
{- Building a tokenizer to use afterwards with the parser. -}

{- Position related -}
type Position = (Integer, Integer)

incrX :: Position -> Position
incrX = incrXx 1

incrXx :: Integer -> Position -> Position
incrXx i (x, y) = (x + i, y)

incrY :: Position -> Position
incrY = incrYy 1

incrYy :: Integer -> Position -> Position
incrYy i (x, y) = (x, y + i)

offToken :: Char -> Token
offToken c = Token c (0, 0)
------------------------------

{- Token related -}
data Token    = Token Char Position

{- Instances -}
instance Show Token where
  show (Token c p) = show c ++ " " ++ show p

instance Eq Token where
  Token c1 _ == Token c2 _ = c1 == c2
  Token c1 _ /= Token c2 _ = c1 /= c2

appToken :: (Char -> a) -> Token -> a
appToken f (Token c _) = f c

isDigit :: Token -> Bool
isDigit token = appToken Data.Char.isDigit token

isLetter :: Token -> Bool
isLetter token = appToken Data.Char.isLetter token

isSpace :: Token -> Bool
isSpace token = appToken Data.Char.isSpace token
------------------------------

tokenizer' :: Position -> String -> [Token]
tokenizer' _   []            = []
tokenizer' pos ('\n':string) =
  (Token '\n' pos):tokenizer' (incrY pos) string
tokenizer' pos (c:string)    =
  (Token c pos):tokenizer' (incrX pos) string

tokenizer :: String -> [Token]
tokenizer = tokenizer' (1,1)

tokenString :: String -> [Token]
tokenString = map offToken

destoken :: Token -> Char
destoken (Token c _) = c

destokenize :: [Token] -> String
destokenize = map destoken
