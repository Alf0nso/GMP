-- import Tokenizer
--   ( Token(..)
--   , destokenize )
-- import Parser
--   ( Parser(..)
--   , between
--   , (<|>)
--   , chainl1 )
-- import Basics
--   ( spaces
--   , digits
--   , charSymbol
--   , executeParserD
--   )

data Morse = Dot Morse
           | Dash Morse
           | Space Morse
           | End

instance Show Morse where
  show (Dot   rest) = '.':show rest
  show (Dash  rest) = '-':show rest
  show (Space rest) = ' ':show rest
  show End          = []

-- A	.-	N	-.
-- B	-...	O	---
-- C	-.-.	P	.--.
-- D	-..	Q	--.-
-- E	.	R	.-.
-- F	..-.	S	...
-- G	--.	T	-
-- H	....	U	..-
-- I	..	V	...-
-- J	.---	W	.--
-- K	-.-	X	-..-
-- L	.-..	Y	-.--
-- M	--	Z	--..

morseSymbols :: Morse -> String
morseSymbols End                               = []
morseSymbols (Dot (Dash (rest)))               = 'a':(morseSymbols rest)
morseSymbols (Dash (Dot (Dot (Dot (rest)))))   = 'b':(morseSymbols rest)
morseSymbols (Dash (Dot (Dash (Dot (rest)))))  = 'c':(morseSymbols rest)
morseSymbols (Dash (Dot (Dot (rest))))         = 'd':(morseSymbols rest)
morseSymbols (Dot (rest))                      = 'e':(morseSymbols rest)
morseSymbols (Dot (Dot (Dash (Dot (rest)))))   = 'f':(morseSymbols rest)
morseSymbols (Dash (Dash (Dot (rest))))        = 'g':(morseSymbols rest)
morseSymbols (Dot (Dot (Dot (Dot (rest)))))    = 'h':(morseSymbols rest)
morseSymbols (Dot (Dot (rest)))                = 'i':(morseSymbols rest)
morseSymbols (Dot (Dash (Dash (Dash (rest))))) = 'j':(morseSymbols rest)
morseSymbols (Dash (Dot (Dash (rest))))        = 'k':(morseSymbols rest)
morseSymbols (Dot (Dash (Dot (Dot (rest)))))   = 'l':(morseSymbols rest)
morseSymbols (Dash (Dash (rest)))              = 'm':(morseSymbols rest)
morseSymbols (Dash (Dot (rest)))               = 'n':(morseSymbols rest)
morseSymbols (Dash (Dash (Dash (rest))))       = 'o':(morseSymbols rest)
morseSymbols (Dot (Dash (Dash (Dot (rest)))))  = 'p':(morseSymbols rest)
morseSymbols (Dash (Dash (Dot (Dash (rest))))) = 'q':(morseSymbols rest)
morseSymbols (Dot (Dash (Dot (rest))))         = 'r':(morseSymbols rest)
morseSymbols (Dot (Dot (Dot (rest))))          = 's':(morseSymbols rest)
morseSymbols (Dash (rest))                     = 't':(morseSymbols rest)
morseSymbols (Dot (Dot (Dash (rest))))         = 'u':(morseSymbols rest)
morseSymbols (Dot (Dot (Dot (Dash (rest)))))   = 'v':(morseSymbols rest)
morseSymbols (Dot (Dash (Dash (rest))))        = 'w':(morseSymbols rest)
morseSymbols (Dash (Dot (Dot (Dash (rest)))))  = 'x':(morseSymbols rest)
morseSymbols (Dash (Dot (Dash (Dash (rest))))) = 'y':(morseSymbols rest)
morseSymbols (Dash (Dash (Dot (Dot (rest)))))  = 'z':(morseSymbols rest)
morseSymbols (Space (rest))                    = ' ':(morseSymbols rest)

morseSymbols' :: String -> Morse
morseSymbols' ('a':rest) = (Dot (Dash ((Space (morseSymbols' rest)))))
morseSymbols' ('b':rest) = (Dash (Dot (Dot (Dot ((Space (morseSymbols' rest)))))))
morseSymbols' ('c':rest) = (Dash (Dot (Dash (Dot ((Space (morseSymbols' rest)))))))
morseSymbols' ('d':rest) = (Dash (Dot (Dot ((Space (morseSymbols' rest))))))
morseSymbols' ('e':rest) = (Dot ((Space (morseSymbols' rest))))
morseSymbols' ('f':rest) = (Dot (Dot (Dash (Dot ((Space (morseSymbols' rest)))))))
morseSymbols' ('g':rest) = (Dash (Dash (Dot ((Space (morseSymbols' rest))))))
morseSymbols' ('h':rest) = (Dot (Dot (Dot (Dot ((Space (morseSymbols' rest)))))))
morseSymbols' ('i':rest) = (Dot (Dot ((Space (morseSymbols' rest)))))
morseSymbols' ('j':rest) = (Dot (Dash (Dash (Dash ((Space (morseSymbols' rest)))))))
morseSymbols' ('k':rest) = (Dash (Dot (Dash ((Space (morseSymbols' rest))))))
morseSymbols' ('l':rest) = (Dot (Dash (Dot (Dot ((Space (morseSymbols' rest)))))))
morseSymbols' ('m':rest) = (Dash (Dash ((Space (morseSymbols' rest)))))
morseSymbols' ('n':rest) = (Dash (Dot ((Space (morseSymbols' rest)))))
morseSymbols' ('o':rest) = (Dash (Dash (Dash ((Space (morseSymbols' rest))))))
morseSymbols' ('p':rest) = (Dot (Dash (Dash (Dot ((Space (morseSymbols' rest)))))))
morseSymbols' ('q':rest) = (Dash (Dash (Dot (Dash ((Space (morseSymbols' rest)))))))
morseSymbols' ('r':rest) = (Dot (Dash (Dot ((Space (morseSymbols' rest))))))
morseSymbols' ('s':rest) = (Dot (Dot (Dot ((Space (morseSymbols' rest))))))
morseSymbols' ('t':rest) = (Dash ((Space (morseSymbols' rest))))
morseSymbols' ('u':rest) = (Dot (Dot (Dash ((Space (morseSymbols' rest))))))
morseSymbols' ('v':rest) = (Dot (Dot (Dot (Dash ((Space (morseSymbols' rest)))))))
morseSymbols' ('w':rest) = (Dot (Dash (Dash ((Space (morseSymbols' rest))))))
morseSymbols' ('x':rest) = (Dash (Dot (Dot (Dash ((Space (morseSymbols' rest)))))))
morseSymbols' ('y':rest) = (Dash (Dot (Dash (Dash ((Space (morseSymbols' rest)))))))
morseSymbols' ('z':rest) = (Dash (Dash (Dot (Dot ((Space (morseSymbols' rest)))))))
morseSymbols' (' ':rest) = (Space ((Space (morseSymbols' rest))))
morseSymbols' []         = End
morseSymbols' (_:rest)   = (Space (morseSymbols' rest))
