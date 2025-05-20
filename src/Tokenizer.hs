{-# LANGUAGE FlexibleInstances #-}
module Tokenizer
  ( Token(..)
  , TChar, TString
  , offToken
  , Tokenizer.isDigit
  , Tokenizer.isLetter
  , Tokenizer.isSpace
  , tokenizer
  , tokenString
  , destoken
  , destokenize
  ) where

import           Data.Char
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
data Token a  = Token a Position

{- Token related -}
type TChar    = Token Char
type TString  = [TChar]

instance (Eq a) => Eq (Token a) where
  (Token x _) == (Token y _) = x == y

{- Instances -}
instance Show TChar where
  show (Token c p) = show c ++ " " ++ show p

appToken :: (a -> b) -> Token a -> b
appToken f (Token c _) = f c

isDigit :: TChar -> Bool
isDigit = appToken Data.Char.isDigit

isLetter :: TChar -> Bool
isLetter = appToken Data.Char.isLetter

isSpace :: TChar -> Bool
isSpace = appToken Data.Char.isSpace

------------------------------

tokenizer' :: Position -> String -> TString
tokenizer' _   []            = []
tokenizer' pos ('\n':string) =
  Token '\n' pos:tokenizer' (incrY pos) string
tokenizer' pos (c:string)    =
  Token c pos:tokenizer' (incrX pos) string

tokenizer :: String -> TString
tokenizer = tokenizer' (1,1)

tokenString :: String -> TString
tokenString = map offToken

destoken :: TChar -> Char
destoken (Token c _) = c

destokenize :: [TChar] -> String
destokenize = map destoken

