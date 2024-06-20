module Basics
  ( space
  , digit
  , letter
  , spaces
  , digits
  , letters
  , charSymbol
  , stringSymbols
  , executeParserD
  , executeParserDWR
  ) where
import Tokenizer
  ( Token(..)
  , isSpace
  , isDigit
  , isLetter
  , offToken
  , tokenizer
  , tokenString
  )
import Parser
  ( Parser(..)
  , satisfy
  , many
  , many1
  , symbol
  , string
  )
import Debugger
  ( debuggerParse
  , debuggerParseWR
  )

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

stringSymbols :: String -> Parser Token [Token]
stringSymbols str = string (tokenString str) <* spaces
------------------------------------------------------------

{- Executing parsers -}
executeParserD :: Show p => Parser Token p -> String -> IO ()
executeParserD parser str = debuggerParse (tokenizer str) parser

executeParserDWR :: ((p, [Token]) -> IO ()) -> Parser Token p -> String -> IO ()
executeParserDWR fun parser str = debuggerParseWR fun (tokenizer str) parser
------------------------------------------------------------------
