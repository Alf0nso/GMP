import Tokenizer
  ( Token(..) )
import Parser
  ( Parser(..)
  , between
  , sepBy )
import Basics
  ( spaces
  , letters
  , charSymbol
  , executeParserD
  )

lbracket, rbracket :: Parser Token Token
lbracket = charSymbol '[' <* spaces
rbracket = spaces *> charSymbol ']'

list :: Parser Token [[Token]]
list = between lbracket things rbracket

letters' :: Parser Token [Token]
letters' = letters <* spaces

things :: Parser Token [[Token]]
things = letters' `sepBy` (charSymbol ',')

main :: IO ()
main = getLine >>= executeParserD list
