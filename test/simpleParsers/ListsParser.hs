import Tokenizer
  ( TChar
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

lbracket, rbracket :: Parser TChar TChar
lbracket = charSymbol '[' <* spaces
rbracket = spaces *> charSymbol ']'

comma :: Parser TChar TChar
comma    = charSymbol ',' <* spaces

letters' :: Parser TChar List
letters' = do l <- between spaces letters spaces
              return $ Atom (destokenize l)

list :: Parser TChar List
list = Cons <$> sepBy expr comma

expr :: Parser TChar List
expr = letters' <|> do _ <- lbracket
                       x <- list
                       _ <- rbracket
                       return x

main :: IO ()
main = executeParserD expr "[a, b, [aa, bb], a, c]"
