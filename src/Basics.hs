module Basics
  ( space
  , digit
  , spaces
  , digits
  , charSymbol
  ) where
import Tokenizer
  ( Token(..)
  , isSpace
  , isDigit
  , offToken
  )
import Parser
  ( Parser(..)
  , satisfy
  , many
  , many1
  , symbol
  )



{- Basic token functions -}
space, digit :: Parser Token Token
space  = satisfy isSpace
digit  = satisfy isDigit

spaces, digits :: Parser Token [Token]
spaces = many space
digits = many1 digit

charSymbol :: Char -> Parser Token Token
charSymbol chr = symbol chr offToken <* spaces
------------------------------------------------------------

{- Debugging basics -}

------------------------------------------------------------
