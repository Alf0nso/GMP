data Algebra = AND     Algebra Algebra
             | OR      Algebra Algebra
             | NOT     Algebra
             | Bool    B

data B = T
       | F
       | Var String

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

-- (∧), conjunction, and :: B -> B -> B
-- conjunction F F = F
-- conjunction T F = F
-- conjunction F T = T
-- conjunction T T = T
-- and             = conjunction
-- (∧)             = conjunction
-- 
-- (∨), disjunction, or :: B -> B -> B
-- disjunction F F = F
-- disjunction T F = T
-- disjunction F T = T
-- disjunction T T = T
-- or              = disjunction
-- (∨)             = disjunction
-- 
-- (¬), negation, not :: B -> B
-- negation T = F
-- negation F = T
-- not        = negation
-- (¬)        = negation
-- 
