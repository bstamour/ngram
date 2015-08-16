{-# LANGUAGE DataKinds #-}

module Text.WordStream.Tests where

import Text.WordStream

import Data.Char
import Control.Monad
import Control.Monad.Random

-------------------------------------------------------------------------------------
-- Testing stuff

clean :: String -> String
clean = map toLower . filter isAlpha

-- Simple test.
test :: String -> Int -> IO String
test input sz = do
  stuff <- readFile "/home/bryan/Code/james.txt"
  let cleaned = map clean $ words stuff
  let table   = newChain :: Chain 3 String
  let trained = train cleaned table
  let gram = makeGram (words input)
  case gram of
    Nothing -> error "Not enough input"
    Just gram' -> do
      result <- evalRandIO $ generate trained gram' sz
      return $ unwords result

-- Example of how one can use dependent types. The size of the n-grams in the
-- chain is a part of the type itself, but the value is read in at runtime.
test2 :: IO String
test2 = do
  -- First read the required params.
  putStr "Enter size of n-grams: "
  sz <- liftM read getLine
  putStr "Enter the starting phrase: "
  phrase <- getLine
  putStr "Enter the number of words to generate: "
  n <- liftM read getLine

  -- Get the training material.
  stuff <- readFile "/home/bryan/Code/james.txt"
  let cleaned = map clean $ words stuff

  -- Create a chain of n-grams where n = sz (read at runtime).
  withChain sz $ \chain -> do
    let trained = train cleaned chain
    let gram = makeGram (words phrase)
    case gram of
      Nothing -> error "Not enough input"
      Just gram' -> do
        result <- evalRandIO $ generate trained gram' n
        return $ unwords result
