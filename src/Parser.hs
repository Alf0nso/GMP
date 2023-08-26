{-# LANGUAGE LambdaCase #-}
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
  , chainl, chainl1
  , sepBy, sepBy1
  , between
  , (<|>)
  , some
  ) where

import Prelude hiding (any)
import Control.Applicative (Alternative
                           , empty
                           , (<|>)
                           , some
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
             deriving (Eq)

instance (Show i) => Show (Error i) where
  show EndOfInput = "End of input"
  show (ExpectedEndOfInput i) =
    "Expected end of input but got " ++ show i
  show ExpectedSomething =
    "Expected something"
  show (Unexpected i) =
    "Unexpected " ++ show i
  show (Expected i1 i2) =
    "Expected " ++ show i1 ++
    " but got " ++ show i2
  show Empty = "Empty"
  show (NoMatch str) =
    "No match for " ++ str

newtype Parser i o =
  Parser { parse :: [i] -> Either [Error i] (o, [i]) }

token :: (i -> Error i) -> (i -> Bool) -> Parser i i
token err predicate =
  Parser $ \case
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
string = foldr (\c -> (<*>) ((:) <$> char c)) (pure [])

eof :: Parser i ()
eof = Parser $ \case
  []    -> Right ((), [])
  (t:_) -> Left [ExpectedEndOfInput t]

any :: Parser i i
any = Parser $ \case
  t:rest -> Right (t, rest)
  []     -> Left [ExpectedSomething]
--------------------------------------

{- Backtracking -}
try :: Parser i o -> Parser i o
try p = Parser $ \input -> case parse p input of
  Left err -> Left err
  success  -> success

choice
  :: (Foldable t, Eq i) => String -> t (Parser i a) -> Parser i a
choice expected =
  foldr (<|>) (Parser $ \_ -> Left [NoMatch expected])

between :: Applicative f => f a -> f b -> f c -> f b
between p1 p2 p3 = p1 *> p2 <* p3
-------------------------------------------------------------------

{- Repetition -}
many, many1 :: (Alternative f, Monad f) => f a -> f [a]
many  p = many1 p <|> return []
many1 p = liftA2 (:) p $ many p

sepBy, sepBy1 :: (Alternative f, Monad f) => f a -> f b -> f [a]
sepBy  p s = sepBy1 p s <|> pure []
sepBy1 p s = (:) <$> p <*> many (s *> p)


chainl :: (Monad f, Alternative f) =>
          f a -> f (a -> a -> a) -> a -> f a
chainl p op x = chainl1 p op <|> return x

chainl1 :: (Monad m, Alternative m) => m b -> m (b -> b -> b) -> m b
chainl1 p op = do x <- p
                  rest x
                    where
                      rest x = do f <- op
                                  y <- p
                                  rest (f x y)
                                    <|> return x
                  
------------------------------------------------------------------
