module Text.NGram
       ( Gram
       , FreqTable
       , newTable
       , toList
       , fromList
       , loadFromFile
       , saveToFile
       , makeTable
       , train
       , getNext
       , stream
       , getNextN
       , generate
       ) where

import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import Control.Monad.Random hiding (fromList)
import qualified Data.Map as M


type Gram a = [a]
newtype FreqTable a = FT { unFT :: M.Map (Gram a) [(a, Int)] }


newTable :: FreqTable a
newTable = FT M.empty

toList :: FreqTable a -> [(Gram a, [(a, Int)])]
toList = M.toList . unFT

fromList ::Ord a =>  [(Gram a, [(a, Int)])] -> FreqTable a
fromList = FT . M.fromList


saveToFile :: Show a => FilePath -> FreqTable a -> IO ()
saveToFile filename = writeFile filename . show . toList

loadFromFile :: (Ord a, Read a) => FilePath -> IO (FreqTable a)
loadFromFile = liftM fromList . liftM read . readFile


-- | Create a new table from scratch.
makeTable :: (Eq a, Ord a) => Int -> [a] -> FreqTable a
makeTable n words = train n words newTable


-- | Train a table on some input, returning a new table.
train :: (Eq a, Ord a) => Int -> [a] -> FreqTable a -> FreqTable a
train n words@(w:ws) table
  | length stuff < n + 1 = table
  | otherwise            = train n ws updated
  where
    stuff   = take (n + 1) words
    updated = FT $ M.insertWith combine (take n stuff) [(last stuff, 1)] $ unFT table

    combine new [] = new
    combine new@[(w', _)] ((w, f):rest)
      | w == w'   = (w, f + 1) : rest
      | otherwise = (w, f) : combine new rest


-- | Generate one new word from a table.
getNext :: (RandomGen g, Ord a) => FreqTable a -> Gram a -> Rand g (Maybe a)
getNext table gram = case M.lookup gram (unFT table) of
  Nothing    -> return Nothing
  Just words -> do
    let total = sum . map snd $ words
    n <- getRandomR (0, total - 1)
    return $ findNext words n 0
  where
    findNext [] _ _ = Nothing
    findNext ((w, f) : ws) n low
      | n - low < f = Just w
      | otherwise   = findNext ws n (low + f)


-- | Generate a lazy stream of words.
stream :: (RandomGen g, Ord a) => FreqTable a -> Gram a -> Rand g [a]
stream table gram = do
  current <- getNext table gram
  case current of
    Nothing -> return []
    Just w -> do
      let updated = tail gram ++ [w]
      rest <- stream table updated
      return $ w : rest


-- | Get N words.
getNextN :: (RandomGen g, Ord a) => FreqTable a -> Gram a -> Int -> Rand g [a]
getNextN table gram n = take n <$> (stream table gram)


-- | Get N words, with the starting words tacked onto the front.
generate :: (Ord a, RandomGen g) => FreqTable a -> Gram a -> Int -> Rand g [a]
generate table gram n = (gram ++) <$> getNextN table gram (n - length gram)


clean :: String -> String
clean = map toLower . filter isAlpha


-- Testing function.
test :: String -> Int -> IO String
test input sz = do
  stuff <- readFile "/home/bryan/Code/james.txt"
  let table = train 2 (map clean $ words stuff) newTable
  fmap unwords . evalRandIO $ generate table (words input) sz
