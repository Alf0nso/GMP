import Tokenizer
  ( Token(..)
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
true' :: Parser Token Algebra
true' = do _ <- between spaces (stringSymbols "true" <|> stringSymbols "⊤") spaces
           return (Bool T)

false' :: Parser Token Algebra
false' = do _ <- between spaces (stringSymbols "false" <|> stringSymbols "⊥") spaces
            return (Bool F)

var' :: Parser Token Algebra
var' = do l <- between spaces letters spaces
          return $ v (destokenize l)

bools' :: Parser Token Algebra
bools' = true' <|> false' <|> var'

lparen, rparen :: Parser Token Token
lparen = charSymbol '(' <* spaces
rparen = spaces *> charSymbol ')'

and'', or'' :: Parser Token (Algebra -> Algebra -> Algebra)
not''       :: Parser Token (Algebra -> Algebra)
and'' = stringSymbols "and" <|> stringSymbols "∧" >> return AND
not'' = stringSymbols "not" <|> stringSymbols "¬" >> return NOT
or''  = stringSymbols "or"  <|> stringSymbols "∨" >> return OR

op :: Parser Token (Algebra -> Algebra -> Algebra)
op = and'' <|> or''

term, alge, unar :: Parser Token Algebra
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

test_parser :: IO ()
test_parser = do alg <- generate $ algebra 5
                 out <- executeParserDR term (show alg)
                 case out of
                   Just x  -> putStrLn ("Reduced: " ++ show (reduce x))
                   Nothing -> putStrLn "Could not solve the parsing action."
