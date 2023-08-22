module Parser
  ( Parser(..)
  , token
  , satisfy
  , char
  , string
  , eof
  , any
  , try
  , choice
  , many, many1
  , sepBy, sepBy1
  , between
  ) where

import Prelude hiding (any)
import Control.Applicative (Alternative
                           , empty
                           , (<|>)
                           , liftA2)
import Data.List (nub)

{- Types and Data types -}
data Error i = EndOfInput
             | ExpectedEndOfInput i
             | ExpectedSomething
             | Unexpected i
             | Expected i i
             | Empty
             | NoMatch String
             deriving (Show, Eq)

newtype Parser i o =
  Parser { parse :: [i] -> Either [Error i] (o, [i]) }

token :: (i -> Error i) -> (i -> Bool) -> Parser i i
token err predicate = Parser $ \input -> case input of
  [] -> Left [EndOfInput]
  test : rest
    | predicate test -> Right (test, rest)
    | otherwise      -> Left [err test]
------------------------------------------------------------

{- Instances -}
instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Left err -> Left err
      Right (output, rest) -> Right (f output, rest)

instance Applicative (Parser i) where
  pure a = Parser $ \input -> Right (a, input)

  Parser p1 <*> Parser p2 = Parser $ \input ->
    case p1 input of
      Left err -> Left err
      Right (p1', rest) ->
        case p2 rest of
          Left err -> Left err
          Right (output, rest') -> Right (p1' output, rest')

instance Monad (Parser i) where
  return = pure
  
  Parser pars >>= cont = Parser $ \input ->
    case pars input of
      Left err -> Left err
      Right (out, rest) ->
        let Parser pars' = cont out in pars' rest

instance (Eq i) => Alternative (Parser i) where
  empty = Parser $ \_ -> Left [Empty]

  Parser p1 <|> Parser p2 = Parser $ \input ->
    case p1 input of
      Left err -> case p2 input of
                   Left err'         -> Left $ nub $ err <> err'
                   Right (out, rest) -> Right (out, rest)
      Right (out, rest) -> Right (out, rest)
------------------------------------------------------------


{- Elementary Parsers -}
satisfy :: (i -> Bool) -> Parser i i
satisfy = token Unexpected

char :: Eq i => i -> Parser i i
char i = token (Expected i) (== i)

string :: Eq i => [i] -> Parser i [i]
string []  = pure []
string (c:str) = (:) <$> char c <*> string str

eof :: Parser i ()
eof = Parser $ \input -> case input of
  []    -> Right ((), [])
  (t:_) -> Left [ExpectedEndOfInput t]

any :: Parser i i
any = Parser $ \input -> case input of
  t:rest -> Right (t, rest)
  []     -> Left [ExpectedSomething]
--------------------------------------

{- Backtracking -}
try :: Parser i i -> Parser i i
try p = Parser $ \input -> case parse p input of
  Left err -> Left err
  success  -> success

choice :: (Eq i) => String -> [Parser i i] -> Parser i i
choice expected = foldr (<|>) (Parser $ \_ -> Left [NoMatch expected])

between :: Parser i i -> Parser i i -> Parser i i -> Parser i i
between p1 p2 p3 = p1 *> p2 <* p3
----------------------------------------------------------------------

{- Repetition -}
many, many1 :: (Eq i) => Parser i i -> Parser i [i]
many  p = many1 p <|> return []
many1 p = liftA2 (:) p $ many p

sepBy, sepBy1 :: (Eq i) => Parser i i -> Parser i f -> Parser i [i]
sepBy  p s = sepBy1 p s <|> pure []
sepBy1 p s = liftA2 (:) p $ many (s >> p)
-----------------------------------------
