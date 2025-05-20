import Tokenizer
  ( TChar
  , destokenize )
import Parser
  ( Parser(..)
  , between
  , (<|>)
  , chainl1 )
import Basics
  ( spaces
  , digits
  , charSymbol
  , executeParserD
  )

digits' :: Parser TChar Double
digits' = do
  _ <- spaces
  c <- digits
  _ <- spaces
  return (read $ destokenize c)

lparen, rparen :: Parser TChar TChar
lparen = charSymbol '(' <* spaces
rparen = spaces *> charSymbol ')'

add', sub', mul', div' :: (Fractional a) => Parser TChar (a -> a -> a)
add' = charSymbol '+' >> return (+)
sub' = charSymbol '-' >> return (-)
mul' = charSymbol '*' >> return (*)
div' = charSymbol '/' >> return (/)

addOp :: (Fractional a) => Parser TChar (a -> a -> a)
addOp = add' <|> sub'

mulOp :: (Fractional a) => Parser TChar (a -> a -> a)
mulOp = mul' <|> div'

expr, term, factor :: Parser TChar Double
expr   = term   `chainl1` addOp
term   = factor `chainl1` mulOp
factor = between lparen expr rparen <|> digits'

main :: IO ()
main = getLine >>= executeParserD expr
