{-# LANGUAGE LambdaCase #-}
module Debugger
  ( solveParser
  , debuggerParse
  , debuggerParseWR
  , solveParserReturn
  , debuggerParseReturn
  , solveParserWithRight ) where

import Parser ( Parser(..) )
import Errors ( Error(..)
              , printErrors )

debuggerParse :: (Show t, Show p) => [t] -> Parser t p -> IO ()
debuggerParse tokens parser = solveParser $ parse parser tokens

debuggerParseReturn :: (Show t, Show p) => [t] -> Parser t p -> IO (Maybe p)
debuggerParseReturn tokens parser = solveParserReturn $ parse parser tokens

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

solveParserReturn :: (Show t, Show p) => Either [Error t] (p, [t]) -> IO (Maybe p)
solveParserReturn = \case
  Left err             -> (putStrLn $ printErrors err) >> return Nothing
  Right (parsed, left) -> putStrLn ("Parsing output: " ++
                                    show parsed ++ "\n" ++
                                    "Left: " ++ show left) >> return (Just parsed)
