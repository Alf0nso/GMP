{-# LANGUAGE LambdaCase #-}
module Debugger
  ( debuggerParse
  , solveParser ) where

import Parser ( Parser(..) )
import Errors ( Error(..)
              , printErrors )

debuggerParse :: (Show t, Show p) => [t] -> Parser t p -> IO ()
debuggerParse tokens parser = solveParser $ parse parser tokens

solveParser :: (Show t, Show p) => Either [Error t] (p, [t]) -> IO ()
solveParser = \case
  Left err             -> putStrLn $ printErrors err
  Right (parsed, left) -> putStrLn $ "Parsing output: " ++ show parsed
                         ++ "\n" ++ "Left: " ++ show left
