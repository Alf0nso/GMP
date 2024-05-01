module Basics
  ( space
  , digit
  , letter
  , spaces
  , digits
  , letters
  , charSymbol
  , executeParserD
  ) where
import Tokenizer
  ( Token(..)
  , isSpace
  , isDigit
  , isLetter
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
space, digit, letter :: Parser Token Token
space  = satisfy isSpace
digit  = satisfy isDigit
letter = satisfy isLetter

spaces, digits, letters :: Parser Token [Token]
spaces  = many space
digits  = many1 digit
letters = many1 letter

charSymbol :: Char -> Parser Token Token
charSymbol chr = symbol chr offToken <* spaces
------------------------------------------------------------

{- Executing parsers -}
executeParserD :: Show p => Parser Token p -> String -> IO ()
executeParserD parser str = debuggerParse (tokenizer str) parser
------------------------------------------------------------------
