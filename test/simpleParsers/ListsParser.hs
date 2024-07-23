import Tokenizer
  ( Token(..)
  , destokenize
  )
import Parser
  ( Parser(..)
  , between
  , (<|>)
  , sepBy )
import Basics
  ( spaces
  , letters
  , charSymbol
  , executeParserD
  )

data List = Atom String
          | Cons [List]
          deriving (Show)

lbracket, rbracket :: Parser Token Token
lbracket = charSymbol '[' <* spaces
rbracket = spaces *> charSymbol ']'

comma :: Parser Token Token
comma    = charSymbol ',' <* spaces

letters' :: Parser Token List
letters' = do l <- between spaces letters spaces
              return $ Atom (destokenize l)

list :: Parser Token List
list = Cons <$> sepBy expr comma

expr :: Parser Token List
expr = letters' <|> do _ <- lbracket
                       x <- list
                       _ <- rbracket
                       return x

main :: IO ()
main = executeParserD expr "[a, b, [aa, bb], a, c]"
