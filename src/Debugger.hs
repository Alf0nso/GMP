{-# LANGUAGE LambdaCase #-}
module Debugger
  ( debuggerParse
  , solveParser
  , solveParserWithRight) where

import Parser ( Parser(..) )
import Errors ( Error(..)
              , printErrors )

debuggerParse :: (Show t, Show p) => [t] -> Parser t p -> IO ()
debuggerParse tokens parser = solveParser $ parse parser tokens

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
