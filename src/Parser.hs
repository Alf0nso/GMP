module Parser
  ( Parser(..)
  , satisfy
  , char
  , string
  ) where

import Prelude hiding (any)

{- Types and Data types -}
data Error i = EndOfInput
             | Unexpected i
             | Empty
             deriving (Show, Eq)

newtype Parser i o =
  Parser { parse :: [i] -> Either [Error i] (o, [i]) }
                     deriving (Functor)
------------------------------------------------------------

{- Instances -}
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
------------------------------------------------------------

{- Elementary Parsers -}
satisfy :: (i -> Bool) -> Parser i i
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    test:rest | predicate test -> Right (test, rest)
              | otherwise      -> Left [Unexpected test]

char :: Eq i => i -> Parser i i
char i = satisfy (== i)

string :: Eq i => [i] -> Parser i [i]
string []  = pure []
string (c:str) = (:) <$> char c <*> string str
