module BooleanAlgebra
  ( parser_on_random
  , parser
  , t, f
  , BooleanAlgebra.and
  , BooleanAlgebra.or
  , BooleanAlgebra.not
  ) where

import Tokenizer
  ( TChar
  , destokenize )
import Parser
  ( Parser(..)
  , between
  , (<|>)
  , chainl1 )
import Basics
  ( spaces
  , letters
  , charSymbol
  , stringSymbols
  , executeParserDR )
import Test.QuickCheck

--------------------------------
-- Parser for boolean algebra --
--------------------------------
true' :: Parser TChar Algebra
true' = do _ <- between spaces (stringSymbols "true" <|>
                               stringSymbols "⊤"    <|>
                               stringSymbols "\8868" ) spaces
           return (Bool T)

false' :: Parser TChar Algebra
false' = do _ <- between spaces (stringSymbols "false" <|>
                                stringSymbols "⊥"     <|>
                                stringSymbols "\8869" ) spaces
            return (Bool F)

var' :: Parser TChar Algebra
var' = do l <- between spaces letters spaces
          return $ v (destokenize l)

bools' :: Parser TChar Algebra
bools' = true' <|> false' <|> var'

lparen, rparen :: Parser TChar TChar
lparen = charSymbol '(' <* spaces
rparen = spaces *> charSymbol ')'

and'', or'' :: Parser TChar (Algebra -> Algebra -> Algebra)
not''       :: Parser TChar (Algebra -> Algebra)
and'' = stringSymbols "and" <|> stringSymbols "∧" <|> stringSymbols "\8743"
        >> return AND
not'' = stringSymbols "not" <|> stringSymbols "¬" <|> stringSymbols "\172"
        >> return NOT
or''  = stringSymbols "or"  <|> stringSymbols "∨" <|> stringSymbols "\8744"
        >> return OR

op :: Parser TChar (Algebra -> Algebra -> Algebra)
op = and'' <|> or''

term, alge, unar :: Parser TChar Algebra
term = unar `chainl1` op
alge = between lparen term rparen <|> bools'
unar = (not'' <*> unar) <|> alge

data Algebra = AND     Algebra Algebra
             | OR      Algebra Algebra
             | NOT     Algebra
             | Bool    B
             deriving Eq

data B = T
       | F
       | Var String
       deriving Eq

t,f :: Algebra
t = Bool T
f = Bool F

v :: String -> Algebra
v = Bool . Var

instance Show Algebra where
  show (AND  (Bool b1) (Bool b2)) = show b1 ++ " ∧ " ++ show b2
  show (AND   a1       (Bool b2)) = "(" ++ show a1 ++ ") ∧ " ++ show b2
  show (AND  (Bool b1)  a2)       = show b1 ++ " ∧ (" ++ show a2 ++ ")"
  show (AND   a1        a2)       = "(" ++ show a1 ++ ") ∧ (" ++ show a2 ++ ")"
  show (OR   (Bool b1) (Bool b2)) = show b1 ++ " ∨ " ++ show b2
  show (OR    a1       (Bool b2)) = "(" ++ show a1 ++ ") ∨ " ++ show b2
  show (OR   (Bool b1)  a2)       = show b1 ++ " ∨ (" ++ show a2 ++ ")"
  show (OR    a1        a2)       = "(" ++ show a1 ++ ") ∨ (" ++ show a2 ++ ")"
  show (NOT  (Bool b))            = "¬" ++ show b
  show (NOT   a)                  = "¬(" ++ show a ++ ")"
  show (Bool  b)                  = show b

instance Show B where
  show T       = "⊤"
  show F       = "⊥"
  show (Var s) = s

instance Arbitrary B where
  arbitrary = oneof [ pure T, pure F, Var <$> smallString ]

smallString :: Gen String
smallString = choose (1,5) >>= \size -> vectorOf size (elements ['a' .. 'z'])

bool :: Gen Algebra
bool = Bool <$> (arbitrary :: Gen B)

not' :: Gen Algebra
not' = NOT <$> algebra 2

and' :: Gen Algebra
and' = AND <$> algebra 2 <*> algebra 2

or' :: Gen Algebra
or' = OR <$> algebra 2 <*> algebra 2

algebra' :: Gen Algebra
algebra' = scale (`div` 2) $ oneof [ bool, not', and', or' ]

algebra :: Int -> Gen Algebra
algebra size
  | size>0 = frequency [(3, algebra'),
                        (1, bool)]
  | otherwise = bool

reduce :: Algebra -> Algebra
reduce (NOT b)          = (¬) b
reduce (AND left right) = (∧) left right
reduce (OR  left right) = (∨) left right
reduce  expr            = expr

(∧), conjunction, and :: Algebra -> Algebra -> Algebra
conjunction left right =
  case (reduce left, reduce right) of
    (Bool F, _)      -> Bool F
    (_, Bool F)      -> Bool F
    (Bool T, right') -> right'
    (left', Bool T)  -> left'
    (left', right')  -> AND left' right'
and                  = conjunction
(∧)                  = conjunction

(∨), disjunction, or :: Algebra -> Algebra -> Algebra
disjunction left right =
  case (reduce left, reduce right) of
    (Bool T, _)      -> Bool T
    (_, Bool T)      -> Bool T
    (Bool F, right') -> right'
    (left', Bool F)  -> left'
    (left', right')  -> OR left' right'
or                   = disjunction
(∨)                  = disjunction

(¬), negation, not :: Algebra -> Algebra
negation expr =
  case reduce expr of
    Bool T      -> Bool F
    Bool F      -> Bool T
    (NOT expr') -> expr'
    expr'       -> NOT expr'
not             = negation
(¬)             = negation

parser_on_random :: IO ()
parser_on_random = do alg <- generate $ algebra 5
                      putStrLn ("Parsing: " ++ show alg)
                      out <- executeParserDR term (show alg)
                      case out of
                        Just x  -> putStrLn ("Reduced: " ++ show (reduce x))
                        Nothing -> putStrLn "Could not solve the parsing action."

parser :: String -> IO ()
parser s = do putStrLn ("Parsing: " ++ show s)
              out <- executeParserDR term (show s)
              case out of
                Just x  -> putStrLn ("Reduced: " ++ show (reduce x))
                Nothing -> putStrLn "Could not solve the parsing action."
