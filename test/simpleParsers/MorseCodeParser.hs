import Tokenizer
  ( Token(..)
  , destokenize )
import Parser
  ( Parser(..)
  , between
  , (<|>)
  , chainl1
  , many )
import Basics
  ( spaces
  , stringSymbols
  , executeParserD
  )

data Morse = Dot Morse
           | Dash Morse
           | Space Morse
           | End

instance Show Morse where
  show (Dot   rest) = '.':show rest
  show (Dash  rest) = '-':show rest
  show (Space rest) = ' ':show rest
  show End          = []

morseSymbols :: Morse -> String
morseSymbols End                               = []
morseSymbols (Dot (Dot (Dot (Dot (rest)))))    = 'h':(morseSymbols rest) -- H    ....
morseSymbols (Dot (Dot (Dot (Dash (rest)))))   = 'v':(morseSymbols rest) -- V    ...-
morseSymbols (Dot (Dot (Dot (rest))))          = 's':(morseSymbols rest) -- S    ... 
morseSymbols (Dot (Dot (Dash (Dot (rest)))))   = 'f':(morseSymbols rest) -- F    ..-.
morseSymbols (Dot (Dot (Dash (rest))))         = 'u':(morseSymbols rest) -- U    ..- 
morseSymbols (Dot (Dot (rest)))                = 'i':(morseSymbols rest) -- I    ..  
morseSymbols (Dot (Dash (Dot (Dot (rest)))))   = 'l':(morseSymbols rest) -- L    .-..
morseSymbols (Dot (Dash (Dash (Dash (rest))))) = 'j':(morseSymbols rest) -- J    .---
morseSymbols (Dot (Dash (Dash (Dot (rest)))))  = 'p':(morseSymbols rest) -- P    .--.
morseSymbols (Dot (Dash (Dash (rest))))        = 'w':(morseSymbols rest) -- W	.--
morseSymbols (Dot (Dash (Dot (rest))))         = 'r':(morseSymbols rest) -- R	.-.
morseSymbols (Dot (Dash (rest)))               = 'a':(morseSymbols rest) -- A	.-
morseSymbols (Dot (rest))                      = 'e':(morseSymbols rest) -- E	.
morseSymbols (Dash (Dash (Dash (rest))))       = 'o':(morseSymbols rest) -- O	---
morseSymbols (Dash (Dash (Dot (Dot (rest)))))  = 'z':(morseSymbols rest) -- Z	--..
morseSymbols (Dash (Dash (Dot (Dash (rest))))) = 'q':(morseSymbols rest) -- Q	--.-
morseSymbols (Dash (Dash (Dot (rest))))        = 'g':(morseSymbols rest) -- G	--.
morseSymbols (Dash (Dash (rest)))              = 'm':(morseSymbols rest) -- M	--
morseSymbols (Dash (Dot (Dot (Dot (rest)))))   = 'b':(morseSymbols rest) -- B	-...
morseSymbols (Dash (Dot (Dot (Dash (rest)))))  = 'x':(morseSymbols rest) -- X	-..-
morseSymbols (Dash (Dot (Dot (rest))))         = 'd':(morseSymbols rest) -- D	-..
morseSymbols (Dash (Dot (Dash (Dash (rest))))) = 'y':(morseSymbols rest) -- Y	-.--
morseSymbols (Dash (Dot (Dash (Dot (rest)))))  = 'c':(morseSymbols rest) -- C	-.-.
morseSymbols (Dash (Dot (Dash (rest))))        = 'k':(morseSymbols rest) -- K	-.-
morseSymbols (Dash (Dot (rest)))               = 'n':(morseSymbols rest) -- N	-.
morseSymbols (Dash (rest))                     = 't':(morseSymbols rest) -- T	-
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

{- Parsing section -}
h, v, s, f, u, i, l, j, p, w, r, a, e :: Parser Token Char
o, z, q, g, m, b, x, d, y, c, k, n, t :: Parser Token Char
h = stringSymbols "...." >> return 'h'
v = stringSymbols "...-" >> return 'v'
s = stringSymbols "..."  >> return 's'
f = stringSymbols "..-." >> return 'f'
u = stringSymbols "..-"  >> return 'u'
i = stringSymbols ".."   >> return 'i'
l = stringSymbols ".-.." >> return 'l'
j = stringSymbols ".---" >> return 'j'
p = stringSymbols ".--." >> return 'p'
w = stringSymbols ".--"  >> return 'w'
r = stringSymbols ".-."  >> return 'r'
a = stringSymbols ".-"   >> return 'a'
e = stringSymbols "."    >> return 'e'
o = stringSymbols "---"  >> return 'o'
z = stringSymbols "--.." >> return 'z'
q = stringSymbols "--.-" >> return 'q'
g = stringSymbols "--."  >> return 'g'
m = stringSymbols "--"   >> return 'm'
b = stringSymbols "-..." >> return 'b'
x = stringSymbols "-..-" >> return 'x'
d = stringSymbols "-.."  >> return 'd'
y = stringSymbols "-.--" >> return 'y'
c = stringSymbols "-.-." >> return 'c'
k = stringSymbols "-.-"  >> return 'k'
n = stringSymbols "-."   >> return 'n'
t = stringSymbols "-"    >> return 't'

morseCode :: Parser Token Char
morseCode = h <|> v <|>
            s <|> f <|>
            u <|> i <|>
            l <|> j <|>
            p <|> w <|>
            r <|> a <|>
            e <|> o <|>
            z <|> q <|>
            g <|> m <|>
            b <|> x <|>
            d <|> y <|>
            c <|> k <|>
            n <|> t

mc :: Parser Token Morse
mc = do morse_code <- many morseCode
        return $ morseSymbols' morse_code
  
main :: IO ()
main = executeParserD mc "---.-..."
