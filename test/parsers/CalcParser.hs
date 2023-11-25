{-# LANGUAGE LambdaCase #-}
import Tokenizer
  ( Token(..)
  , tokenizer
  , isLetter
  , isSpace
  , isDigit
  , offToken
  , destokenize
  , tokenString
  )
import Parser
  ( Parser(..)
  , between
  , choice
  , (<|>)
  , satisfy
  , char
  , string
  , many
  , many1
  , sepBy1
  )
import Errors
  ( Error(..) )

{- A parser for a really basic calculator which
only accepts: numbers, adition, subtraction,
multiplication and division. -}
data CT = Num String
        | Add CT CT
        | Sub CT CT
        | Mul CT CT
        | Div CT CT
        deriving Show

space, digit, letter :: Parser Token Token
space  = satisfy isSpace
letter = satisfy isLetter
digit  = satisfy isDigit

spaces, digits' :: Parser Token [Token]
spaces = many space
digits' = many digit

symbol :: Char -> Parser Token Token
symbol c = char (offToken c)

lparen, rparen :: Parser Token Token
lparen = symbol '(' <* spaces
rparen = spaces *> symbol ')'

add', sub', mul', div' :: Parser Token Token
add' = symbol '+'
sub' = symbol '-'
mul' = symbol '*'
div' = symbol '/'

-- add :: Parser Token CT
-- add = do foldl1 Add <$> sepBy1 term spaces

nums :: Parser Token String
nums = destokenize <$> digits'

digits :: Parser Token CT
digits = Num <$> nums

-- term :: Parser Token CT
-- term = digits <|> add

-- main :: IO ()
-- main = debuggerParse (tokenizer "123") digits
