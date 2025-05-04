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
  , executeParserDR
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
  , debuggerParseReturn
  )

type Tchar   = Token Char
type Tstring = [Token Char]

{- Basic token functions -}
space, digit, letter :: Parser Tchar Tchar
space  = satisfy isSpace
digit  = satisfy isDigit
letter = satisfy isLetter

spaces, digits, letters :: Parser Tchar Tstring
spaces  = many space
digits  = many1 digit
letters = many1 letter

charSymbol :: Char -> Parser Tchar Tchar
charSymbol chr = symbol chr offToken <* spaces

stringSymbols :: String -> Parser Tchar Tstring
stringSymbols str = string (tokenString str) <* spaces
------------------------------------------------------------

{- Executing parsers -}
executeParserD :: Show p => Parser Tchar p -> String -> IO ()
executeParserD parser str = debuggerParse (tokenizer str) parser

executeParserDR :: Show p => Parser Tchar p -> String -> IO (Maybe p)
executeParserDR parser str = debuggerParseReturn (tokenizer str) parser

executeParserDWR :: ((p, Tstring) -> IO ()) -> Parser Tchar p -> String -> IO ()
executeParserDWR fun parser str = debuggerParseWR fun (tokenizer str) parser
------------------------------------------------------------------
