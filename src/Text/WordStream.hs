{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.WordStream
       ( module GHC.TypeLits
       , NGram
       , Chain
       , makeGram
       , newChain
       , makeChain
       , withChain
       , toList
       , fromList
       , loadFromFile
       , saveToFile
       , train
       , getNext
       , stream
       , getNextN
       , generate
       ) where

import GHC.TypeLits
import Data.Proxy
import Control.Monad
import Control.Monad.Random hiding (fromList)
import qualified Data.Map as M

-----------------------------------------------------------------------------------
-- An n-gram that knows its size at compile time. This allows us to force chains
-- to only have one kind of n-gram inside it, instead of relying on lists that can
-- vary in size.

data NGram :: Nat -> * -> * where
  NGram :: [a] -> NGram n a

unGram :: NGram n a -> [a]
unGram (NGram xs) = xs

deriving instance Eq a   => Eq (NGram n a)
deriving instance Ord a  => Ord (NGram n a)
deriving instance Show a => Show (NGram n a)
deriving instance Read a => Read (NGram n a)
deriving instance Foldable (NGram n)

makeGram :: forall n a . KnownNat n => [a] -> Maybe (NGram n a)
makeGram grams | length stuff < amount = Nothing
               | otherwise             = Just $ NGram stuff
  where
    stuff  = take amount grams
    amount = val (Proxy :: Proxy n)

-----------------------------------------------------------------------------------
-- A chain of n-grams is a map from n-grams to frequency tables.

newtype Chain n a = FT { unFT :: M.Map (NGram n a) [(a, Int)] }
                  deriving (Eq, Ord, Show, Read)

-----------------------------------------------------------------------------------
-- Dependently typed functions. Chains carry the size of their n-grams as a part
-- of the type. This function lets you use runtime values as the size.

withChain :: Integer -> (forall n . KnownNat n => Chain n a -> b) -> b
withChain i f = case someNatVal i of
  Just s  -> withChain' s f
  Nothing -> withChain' (SomeNat (Proxy :: Proxy 1)) f

withChain' :: SomeNat -> (forall n . KnownNat n => Chain n a -> b) -> b
withChain' (SomeNat p) f = withChain'' p f

withChain'' :: KnownNat n => Proxy n -> (Chain n a -> b) -> b
withChain'' _ f = f (newChain :: Chain n a)

-----------------------------------------------------------------------------------
-- Loading and saving markov chains.

toList :: Chain n a -> [(NGram n a, [(a, Int)])]
toList = M.toList . unFT

fromList ::Ord a =>  [(NGram n a, [(a, Int)])] -> Chain n a
fromList = FT . M.fromList

saveToFile :: Show a => FilePath -> Chain n a -> IO ()
saveToFile filename = writeFile filename . show . toList

loadFromFile :: (Ord a, Read a) => FilePath -> IO (Chain n a)
loadFromFile = liftM fromList . liftM read . readFile

-----------------------------------------------------------------------------------
-- Primary interface.

newChain :: Chain n a
newChain = FT M.empty

-- | Create a new table from scratch.
makeChain :: forall n a . (Eq a, Ord a, KnownNat n) => [a] -> Chain n a
makeChain words = train words newChain

-- | Train a table on some input, returning a new table.
train :: forall n a . (Eq a, Ord a, KnownNat n) => [a] -> Chain n a -> Chain n a
train [] table = table
train words@(w:ws) table | length stuff < sz + 1 = table
                         | otherwise             = train ws updated
  where
    stuff        = take (sz + 1) words
    sz           = val (Proxy :: Proxy n)
    ~(Just gram) = makeGram (take sz stuff) :: Maybe (NGram n a)
    updated      = FT $ M.insertWith combine gram [(last stuff, 1)] $ unFT table

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
    findNext ((w, f) : ws) n low | n - low < f = Just w
                                 | otherwise   = findNext ws n (low + f)

-- | Generate a lazy stream of words.
stream :: (RandomGen g, Ord a) => Chain n a -> NGram n a -> Rand g [a]
stream table gram = do
  current <- getNext table gram
  case current of
    Nothing -> return []
    Just w   -> do
      let updated = tail (unGram gram) ++ [w]
      rest <- stream table (NGram updated)
      return $ w : rest

-- | Get N words.
getNextN :: (RandomGen g, Ord a) => Chain n a -> NGram n a -> Int -> Rand g [a]
getNextN table gram n = take n <$> (stream table gram)

-- | Get N words, with the starting words tacked onto the front.
generate :: (Ord a, RandomGen g) => Chain n a -> NGram n a -> Int -> Rand g [a]
generate table gram n = ((unGram gram) ++) <$> getNextN table gram (n - length gram)

-------------------------------------------------------------------------------------
-- Helper functions.

val :: (Num a, KnownNat n) => Proxy n -> a
val = fromIntegral . natVal
