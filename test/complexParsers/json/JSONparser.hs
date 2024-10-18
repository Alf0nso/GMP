{-
This test was entirely done using the following tutorial:
https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell/
Thank you Abhinav Sarkar, your blog posts are incredible
-}

module JSONparser () where
import qualified Data.Map as Map
import Data.Char (ord)
import Numeric (showHex)
import Test.QuickCheck

import Tokenizer
  ( Token(..)
  , destokenize )
import Parser
  ( Parser(..)
  , between
  , (<|>)
  , ($>)
  , chainl1 )
import Basics
  ( executeParserD
  , stringSymbols
  )

-- Helper functions --
intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

intersperse :: a -> [a] -> [a]
intersperse _   []     = []
intersperse sep (x:xs) = x : prependToAll sep xs

prependToAll :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

showKeyValue :: (Show k) => (String, k) -> String
showKeyValue (k, v) = showJSONString k ++ ": " ++ show v

isControl :: Char -> Bool
isControl c = c `elem` ['\0' .. '\31']

getHexStr40 :: Char -> [Char]
getHexStr40 c = "0000" ++ showHex (ord c) ""

showJSONNonASCIIChar :: Char -> [Char]
showJSONNonASCIIChar c = drop (length (getHexStr40 c) - 4) (getHexStr40 c)

{- Generators -}
hexDigitLetters :: String
hexDigitLetters = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

escapedUnicodeChar :: Gen [Char]
escapedUnicodeChar = ("\\u" ++) <$> vectorOf 4 (elements hexDigitLetters)

stringGen :: Gen String
stringGen = concat <$> listOf
  (oneof [ vectorOf 1 arbitraryUnicodeChar , escapedUnicodeChar ])
----------------------

data JSON = Null
          | JBol Bool
          | JStr String
          | JNum  Integer [Int] Integer
          | JArr [JSON]
          | JObj (Map.Map String JSON)
          deriving (Eq)

instance Show JSON where
  show  Null          = "null"
  show (JBol True   ) = "true"
  show (JBol False  ) = "false"
  show (JStr string ) = showJSONString string
  show (JNum i [] 0 ) = show i
  show (JNum i f  0 ) = show i ++ "." ++ concatMap show f
  show (JNum i [] e ) = show i ++ "e" ++ show e
  show (JNum i f  e ) =
    show i ++ "." ++ concatMap show f ++ show "e" ++ show e
  show (JArr arr    ) = "[" ++ intercalate ", " (map show arr) ++ "]"
  show (JObj obj    ) =
    "{" ++ intercalate ", " (map showKeyValue $ Map.toList obj) ++ "}"


showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

showJSONChar :: Char -> [Char]
showJSONChar '\'' = "'"
showJSONChar '\"' = "\\\""
showJSONChar '\\' = "\\\\"
showJSONChar '/'  = "\\/"
showJSONChar '\b' = "\\b"
showJSONChar '\f' = "\\f"
showJSONChar '\n' = "\\n"
showJSONChar '\r' = "\\r"
showJSONChar '\t' = "\\t"
showJSONChar c
  | isControl c = "\\u" ++ showJSONNonASCIIChar c
  | otherwise = [c]

-- QuickCheck stuff
-- Scalar generatores
nullGen, boolGen, numbGen, striGen :: Gen JSON
nullGen = pure Null
boolGen = JBol <$> arbitrary
numbGen = JNum <$> arbitrary <*> listOf (choose (0,9)) <*> arbitrary
striGen = JStr <$> stringGen

-- Composite generators
array, object :: Int -> Gen JSON
array = fmap JArr . scale (`div` 2) . listOf . fiveVals . (`div` 2)
object n = JObj <$> Map.fromList <$> keysValues n

keysValues :: Int -> Gen [(String, JSON)]
keysValues n = scale (`div` 2) $ listOf $ (,) <$> stringGen <*> (fiveVals (div n 2))

scalar :: [Gen JSON]
scalar    = [nullGen, boolGen, numbGen, striGen]

composite :: Int -> [Gen JSON]
composite n = [array n, object n]

value :: Int -> Int -> Int -> Gen JSON
value s c n = frequency [(s, oneof scalar), (c, oneof (composite n))]

fiveVals :: Int -> Gen JSON
fiveVals n
  | n < 5     = value 4 1 n
  | otherwise = value 1 4 n

-- Parsing
nullP :: Parser Token JSON
nullP = stringSymbols "null" $> Null

boolP :: Parser Token JSON
boolP = stringSymbols ""
