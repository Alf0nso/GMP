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
  ) where

import Prelude hiding (any)
import Control.Applicative (Alternative (..))
import Data.List (nub)
-- import Data.Char
-- import Tokenizer ( Token(..)
--                  , tokenizer )

{- Types and Data types -}
data Error i = EndOfInput
             | ExpectedEndOfInput i
             | ExpectedSomething
             | Unexpected i
             | Expected i i
             | Empty
             | NoMatch i
             deriving (Show, Eq)

newtype Parser i o =
  Parser { parse :: [i] -> Either [Error i] (o, [i]) }
                     deriving (Functor)

token :: (i -> Error i) -> (i -> Bool) -> Parser i i
token err predicate = Parser $ \input -> case input of
  [] -> Left [EndOfInput]
  test : rest
    | predicate test -> Right (test, rest)
    | otherwise      -> Left [err test]
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

try :: Parser i i -> Parser i i
try p = Parser $ \input -> case parse p input of
  Left err -> Left err
  success  -> success

choice :: (Eq i) => i -> [Parser i i] -> Parser i i
choice expected = foldr (<|>) (Parser $ \_ -> Left [NoMatch expected])
------------------------------------------------------------
