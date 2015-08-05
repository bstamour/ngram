{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.WordStream
       ( module GHC.TypeLits
       , NGram
       , Chain
       , newChain
       , toList
       , fromList
       , loadFromFile
       , saveToFile
       , makeChain
       , train
       , getNext
       , stream
       , getNextN
       , generate
       ) where

import GHC.TypeLits

import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import Control.Monad.Random hiding (fromList)
import qualified Data.Map as M


data Proxy (n :: Nat) = Proxy

data NGram (n :: Nat) (a :: *) = NGram { unGram :: [a] }
                                 deriving (Eq, Ord, Show, Read, Foldable)


makeGram :: forall n a . KnownNat n => [a] -> NGram n a
makeGram grams
  | length stuff < amount = error "makeGram: not enough data"
  | otherwise             = NGram stuff
  where
    stuff  = take amount grams
    amount = fromInteger $ natVal (Proxy :: Proxy n)


newtype Chain n a = FT { unFT :: M.Map (NGram n a) [(a, Int)] }
                  deriving (Eq, Ord, Show, Read)


newChain :: Chain n a
newChain = FT M.empty

toList :: Chain n a -> [(NGram n a, [(a, Int)])]
toList = M.toList . unFT

fromList ::Ord a =>  [(NGram n a, [(a, Int)])] -> Chain n a
fromList = FT . M.fromList


saveToFile :: Show a => FilePath -> Chain n a -> IO ()
saveToFile filename = writeFile filename . show . toList

loadFromFile :: (Ord a, Read a) => FilePath -> IO (Chain n a)
loadFromFile = liftM fromList . liftM read . readFile


-- | Create a new table from scratch.
makeChain :: forall n a . (Eq a, Ord a, KnownNat n) => [a] -> Chain n a
makeChain words = train words newChain


-- | Train a table on some input, returning a new table.
train :: forall n a . (Eq a, Ord a, KnownNat n) => [a] -> Chain n a -> Chain n a
train words@(w:ws) table
  | length stuff < sz + 1 = table
  | otherwise             = train ws updated
  where
    stuff   = take (sz + 1) words
    sz      = fromIntegral $ natVal (Proxy :: Proxy n)
    gram    = makeGram (take sz stuff) :: NGram n a
    updated = FT $ M.insertWith combine gram [(last stuff, 1)] $ unFT table

    combine new [] = new
    combine new@[(w', _)] ((w, f):rest)
      | w == w'   = (w, f + 1) : rest
      | otherwise = (w, f) : combine new rest


-- | Generate one new word from a table.
getNext :: (RandomGen g, Ord a) => Chain n a -> NGram n a -> Rand g (Maybe a)
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
stream :: (RandomGen g, Ord a) => Chain n a -> NGram n a -> Rand g [a]
stream table gram = do
  current <- getNext table gram
  case current of
    Nothing -> return []
    Just w -> do
      let updated = tail (unGram gram) ++ [w]
      rest <- stream table (NGram updated)
      return $ w : rest


-- | Get N words.
getNextN :: (RandomGen g, Ord a) => Chain n a -> NGram n a -> Int -> Rand g [a]
getNextN table gram n = take n <$> (stream table gram)


-- | Get N words, with the starting words tacked onto the front.
generate :: (Ord a, RandomGen g) => Chain n a -> NGram n a -> Int -> Rand g [a]
generate table gram n = ((unGram gram) ++) <$> getNextN table gram (n - length gram)


clean :: String -> String
clean = map toLower . filter isAlpha


-- Testing function.
test :: String -> Int -> IO String
test input sz = do
  stuff <- readFile "/home/bryan/Code/james.txt"
  let cleaned = map clean $ words stuff
  let table   = newChain :: Chain 3 String
  let trained = train cleaned table
  result <- evalRandIO $ generate trained (NGram $ words input) sz
  return $ unwords result
