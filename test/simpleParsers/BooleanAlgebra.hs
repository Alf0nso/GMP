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

reduce :: Algebra -> Algebra
reduce (NOT bool)       = (¬) bool
reduce (AND left right) = (∧) left right
reduce (OR  left right) = (∨) left right

(∧), conjunction, and :: Algebra -> Algebra -> Algebra
conjunction (Bool F) (Bool F) = (Bool F)
conjunction (Bool T) (Bool F) = (Bool F)
conjunction (Bool F) (Bool T) = (Bool F)
conjunction (Bool T) (Bool T) = (Bool T)
conjunction  left     right   = conjunction (reduce left) (reduce right)
and             = conjunction
(∧)             = conjunction

(∨), disjunction, or :: Algebra -> Algebra -> Algebra
disjunction (Bool F) (Bool F) = (Bool F)
disjunction (Bool T) (Bool F) = (Bool T)
disjunction (Bool F) (Bool T) = (Bool T)
disjunction (Bool T) (Bool T) = (Bool T)
disjunction  left     right   = disjunction (reduce left) (reduce right)
or              = disjunction
(∨)             = disjunction

(¬), negation, not :: Algebra -> Algebra
negation (Bool T) = (Bool F)
negation (Bool F) = (Bool T)
negation expr     = reduce expr
not        = negation
(¬)        = negation

