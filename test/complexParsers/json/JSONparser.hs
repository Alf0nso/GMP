module JSONparser () where
import qualified Data.Map as Map

-- Helper functions --
intersperse :: a -> [a] -> [a]
intersperse _   []     = []
intersperse sep (x:xs) = x : prependToAll sep xs

prependToAll :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

showKeyValue :: (Show k) => (String, k) -> String
showKeyValue (k, v) = k ++ ": " ++ show v
----------------------

data JSON = Null
          | JBol Bool
          | JStr String
          | JNum  { int :: Integer, frac :: [Int], exponent :: Integer }
          | JArr [JSON]
          | JObj (Map.Map String JSON)
          deriving (Eq)

instance Show JSON where
  show (Null        ) = "null"
  show (JBol True   ) = "true"
  show (JBol False  ) = "false"
  show (JStr string ) = show string
  show (JNum i [] 0 ) = show i
  show (JNum i f  0 ) = show i ++ "." ++ concat (map show f)
  show (JNum i [] e ) = show i ++ "e" ++ show e
  show (JNum i f  e ) = show i ++ "." ++ concat (map show f) ++ show "e" ++ show e
  show (JArr arr    ) = "[" ++ (concat (intersperse ", " (map show arr))) ++ "]"
  show (JObj obj    ) =
    "{" ++ (concat (intersperse ", " (map showKeyValue $ Map.toList obj))) ++ "}"


