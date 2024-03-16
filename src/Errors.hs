module Errors (Error(..)
              , prettyErrors
              , printErrors
              ) where

data Error i = EndOfInput
             | ExpectedEndOfInput i
             | ExpectedSomething
             | Unexpected i
             | Expected i i
             | Empty
             | NoMatch String
             deriving (Eq)

instance (Show i) => Show (Error i) where
  show EndOfInput             = "End of input"
  show (ExpectedEndOfInput i) = "Expected end of input but got "
                                ++ show i
  show ExpectedSomething      = "Expected something"
  show (Unexpected i)         = "Unexpected " ++ show i
  show (Expected i1 i2)       = "Expected " ++ show i1
                                ++ " but got " ++ show i2
  show Empty                  = "Empty"
  show (NoMatch str)          = "No match for " ++ str

prettyErrors :: (Show i) => [Error i] -> String
prettyErrors []     = ""
prettyErrors (e:es) = "| " <> show e <> "\n" <> prettyErrors es 

printErrors :: (Show i) => [Error i] -> String
printErrors errors = "| \n" <> prettyErrors errors
