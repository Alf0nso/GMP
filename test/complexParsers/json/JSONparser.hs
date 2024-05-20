{-
This test was entirely done using the following tutorial:
https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell/
Thank you Abhinav Sarkar, your blog posts are incredible
-}

module JSONparser () where
import qualified Data.Map as Map
import Data.Char (ord)
import Numeric (showHex)

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
----------------------

data JSON = Null
          | JBol Bool
          | JStr String
          | JNum  { int :: Integer, frac :: [Int], exponent :: Integer }
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
