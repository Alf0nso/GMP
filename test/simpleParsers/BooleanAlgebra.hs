import Test.QuickCheck

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

reduce :: Algebra -> Algebra
reduce (NOT bool)       = (¬) bool
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

-- (AND (NOT (OR f (NOT (v "y")))) (NOT (v "x"))) -> (¬(¬y)) ∧ (¬x)
smallString :: Gen String
smallString = choose (1,5) >>= \size -> vectorOf size (elements ['a' .. 'z'])
