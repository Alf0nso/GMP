module Basics
  ( space
  , digit
  , spaces
  , digits
  , charSymbol
  , executeParserD
  ) where
import Tokenizer
  ( Token(..)
  , isSpace
  , isDigit
  , offToken
  , tokenizer
  )
import Parser
  ( Parser(..)
  , satisfy
  , many
  , many1
  , symbol
  )
import Debugger
  ( debuggerParse )

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

{- Executing parsers -}
executeParserD :: Show p => Parser Token p -> String -> IO ()
executeParserD parser str = debuggerParse (tokenizer str) parser
------------------------------------------------------------------
