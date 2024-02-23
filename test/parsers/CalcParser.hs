import Tokenizer
  ( Token(..)
  , tokenizer
  , isSpace
  , isDigit
  , offToken
  , destokenize )
import Parser
  ( Parser(..)
  , between
  , (<|>)
  , satisfy
  , char
  , many
  , chainl1 )
import Debugger
  ( debuggerParse )

space, digit :: Parser Token Token
space  = satisfy isSpace
digit  = satisfy isDigit

spaces, digits' :: Parser Token [Token]
spaces = many space
digits' = many digit

digits :: Parser Token Double
digits = do
  _ <- spaces
  c <- digits'
  _ <- spaces
  return (read $ destokenize c)

symbol :: Char -> Parser Token Token
symbol c = char (offToken c) <* spaces

lparen, rparen :: Parser Token Token
lparen = symbol '(' <* spaces
rparen = spaces *> symbol ')'

add', sub', mul', div' :: (Fractional a) => Parser Token (a -> a -> a)
add' = symbol '+' >> return (+)
sub' = symbol '-' >> return (-)
mul' = symbol '*' >> return (*)
div' = symbol '/' >> return (/)

addOp :: (Fractional a) => Parser Token (a -> a -> a)
addOp = add' <|> sub'

mulOp :: (Fractional a) => Parser Token (a -> a -> a)
mulOp = mul' <|> div'

expr, term, factor :: Parser Token Double
expr   = term   `chainl1` addOp
term   = factor `chainl1` mulOp
factor = between lparen expr rparen <|> digits

main :: IO ()
main = do str <- getLine
          debuggerParse (tokenizer str) expr
