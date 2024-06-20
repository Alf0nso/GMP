{-# LANGUAGE LambdaCase #-}
module Debugger
  ( debuggerParse
  , solveParser
  , debuggerParseWR
  , solveParserWithRight ) where

import Parser ( Parser(..) )
import Errors ( Error(..)
              , printErrors )

debuggerParse :: (Show t, Show p) => [t] -> Parser t p -> IO ()
debuggerParse tokens parser = solveParser $ parse parser tokens

debuggerParseWR :: Show t => ((p, [t]) -> IO ()) -> [t] -> Parser t p -> IO ()
debuggerParseWR fun tokens parser = solveParserWithRight fun $ parse parser tokens

solveParserWithRight :: (Show t) => ((p, [t]) -> IO ())
                     -> Either [Error t] (p, [t]) -> IO ()
solveParserWithRight fun = \case
  Left  err    -> putStrLn $ printErrors err
  Right result -> fun result

solveParser :: (Show t, Show p) => Either [Error t] (p, [t]) -> IO ()
solveParser = \case
  Left err             -> putStrLn $ printErrors err
  Right (parsed, left) -> putStrLn $ "Parsing output: " ++ show parsed
                         ++ "\n" ++ "Left: " ++ show left
