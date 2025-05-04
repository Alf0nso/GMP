{-# LANGUAGE FlexibleInstances #-}
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

offToken :: Char -> Token Char
offToken c = Token c (0, 0)
------------------------------
-- class Tokenizable t where
--   type 

data Token a  = Token a Position
  deriving (Eq)

{- Token related -}
-- data Token    = Token Char Position

{- Instances -}
instance Show (Token Char) where
  show (Token c p) = show c ++ " " ++ show p

appToken :: (a -> b) -> Token a -> b
appToken f (Token c _) = f c

isDigit :: Token Char -> Bool
isDigit = appToken Data.Char.isDigit

isLetter :: Token Char -> Bool
isLetter = appToken Data.Char.isLetter

isSpace :: Token Char -> Bool
isSpace = appToken Data.Char.isSpace

------------------------------

tokenizer' :: Position -> String -> [Token Char]
tokenizer' _   []            = []
tokenizer' pos ('\n':string) =
  Token '\n' pos:tokenizer' (incrY pos) string
tokenizer' pos (c:string)    =
  Token c pos:tokenizer' (incrX pos) string

tokenizer :: String -> [Token Char]
tokenizer = tokenizer' (1,1)

tokenString :: String -> [Token Char]
tokenString = map offToken

destoken :: Token Char -> Char
destoken (Token c _) = c

destokenize :: [Token Char] -> String
destokenize = map destoken

