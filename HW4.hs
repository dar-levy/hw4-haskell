{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW4.hs EqSet.hs EqMap.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4 where

import Data.Char (chr, ord)
import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup (Arg (..))
import EqMap (EqMap)
import EqMap qualified as EM
import EqSet (EqSet)
import EqSet qualified as ES

class Serializable a where
  serialize :: a -> [Int]
  deserialize :: [Int] -> a

instance Serializable Int where
  serialize x = [x]
  deserialize [x] = x
  deserialize _ = error "Invalid input for Int deserialization"

instance Serializable Bool where
  serialize True = [1]
  serialize False = [0]
  deserialize [1] = True
  deserialize [0] = False
  deserialize _ = error "Invalid input for Bool deserialization"

instance Serializable Char where
  serialize x = [ord x]
  deserialize [x] = chr x
  deserialize _ = error "Invalid input for Char deserialization"

instance Serializable a => Serializable (Maybe a) where
  serialize Nothing = [-1]
  serialize (Just x) = 0 : serialize x
  deserialize (-1:_) = Nothing
  deserialize (0:xs) = Just (deserialize xs)
  deserialize _ = error "Invalid input for Maybe deserialization"

instance (Serializable a, Serializable b) => Serializable (a, b) where
  serialize (x, y) = serialize x ++ serialize y
  deserialize xs =
    let (xRest, xs') = splitAt 1 xs -- Take the serialized length of 1 element
        (yRest, _) = splitAt 1 xs' -- Take the serialized length of 1 element
    in (deserialize xRest, deserialize yRest)

instance (Serializable a, Serializable b) => Serializable (Either a b) where
  serialize (Left x) = 0 : serialize x
  serialize (Right y) = 1 : serialize y
  deserialize (0:xs) = Left (deserialize xs)
  deserialize (1:xs) = Right (deserialize xs)
  deserialize _ = error "Invalid input for Either deserialization"

instance Serializable a => Serializable [a] where
  serialize xs = 1 : concatMap (\x -> serialize x ++ [-1]) xs

  deserialize (1:xs) = deserializeList xs
    where
      deserializeList [] = []
      deserializeList ys =
        let (element, rest) = break (== -1) ys
        in deserialize element : deserializeList (drop 1 rest)
  deserialize _ = error "Invalid input for list deserialization"

instance (Serializable a, Eq a) => Serializable (EqSet a) where
  serialize set = 1 : concatMap (\x -> serialize x ++ [-1]) (ES.elems set)

  deserialize (1:xs) = deserializeSet xs
    where
      deserializeSet [] = ES.empty
      deserializeSet ys =
        let (element, rest) = break (== -1) ys
        in ES.insert (deserialize element) (deserializeSet (drop 1 rest))
  deserialize _ = error "Invalid input for EqSet deserialization"

instance (Serializable k, Eq k, Serializable v) => Serializable (EqMap k v) where
  serialize m = 1 : concatMap (\(k, v) -> serialize k ++ serialize v ++ [-1]) (EM.assocs m)

  deserialize (1:xs) = deserializeMap xs
    where
      deserializeMap [] = EM.empty
      deserializeMap ys =
        let (pair, rest) = break (== -1) ys
            (k, vRest) = deserializeElement pair
            v = deserialize vRest
        in EM.insert k v (deserializeMap (drop 1 rest))

      deserializeElement :: [Int] -> (k, [Int])
      deserializeElement zs =
        let (kPrefix, kRest) = splitAt (length (serialize (undefined :: k))) zs
            k = deserialize kPrefix
            vRest = kRest
        in (k, vRest)

  deserialize _ = error "Invalid input for EqMap deserialization"

-- Tests
main :: IO ()
main = do
  -- Test cases for EqMap (Int, Bool)
  let eqMapVal1 = EM.insert 1 True (EM.insert 2 False (EM.insert 3 True EM.empty)) :: EqMap Int Bool
  let eqMapVal2 = EM.empty :: EqMap Int Bool
  let serializedEqMapVal1 = serialize eqMapVal1
  let serializedEqMapVal2 = serialize eqMapVal2
  let deserializedEqMapVal1 = deserialize serializedEqMapVal1 :: EqMap Int Bool
  let deserializedEqMapVal2 = deserialize serializedEqMapVal2 :: EqMap Int Bool
  print $ deserializedEqMapVal1 == eqMapVal1 -- Should be True
  print $ deserializedEqMapVal2 == eqMapVal2 -- Should be True

  -- Test cases for EqMap (Char, Int)
  let eqMapCharVal1 = EM.insert 'a' 1 (EM.insert 'b' 2 (EM.insert 'c' 3 EM.empty)) :: EqMap Char Int
  let eqMapCharVal2 = EM.empty :: EqMap Char Int
  let serializedEqMapCharVal1 = serialize eqMapCharVal1
  let serializedEqMapCharVal2 = serialize eqMapCharVal2
  let deserializedEqMapCharVal1 = deserialize serializedEqMapCharVal1 :: EqMap Char Int
  let deserializedEqMapCharVal2 = deserialize serializedEqMapCharVal2 :: EqMap Char Int
  print $ deserializedEqMapCharVal1 == eqMapCharVal1 -- Should be True
  print $ deserializedEqMapCharVal2 == eqMapCharVal2 -- Should be True

---- Section 3: Metric
--infinity :: Double
--infinity = 1 / 0
--
--class Eq a => Metric a where
--  distance :: a -> a -> Double
--
--instance Metric Double
--instance Metric Int
--instance Metric Char
--
---- Euclidean distance
--instance (Metric a, Metric b) => Metric (a, b)
--
--data ManhattanTuple a b = ManhattanTuple a b deriving Eq
--instance (Metric a, Metric b) => Metric (ManhattanTuple a b)
--
---- Just and Nothing have distance of infinity.
---- Two Justs measure the distance between the two values.
--instance Metric a => Metric (Maybe a)
--
---- Left and Right have a distance of infinity.
---- Same constructores measure the distance between the two values.
--instance (Metric a, Metric b) => Metric (Either a b)
--
---- Lists of different sizes have distance of infinity.
---- Euclidean distance.
--instance Metric a => Metric [a]
--
--newtype ManhattanList a = ManhattanList [a] deriving Eq
--instance Metric a => Metric (ManhattanList a)
--
---- Returns the element with the shortest distance to the input.
---- If there are no numbers whose distance is less than infinity, return Nothing.
--closest :: Metric a => a -> [a] -> Maybe a
---- Similar to the above, but uses a function move the element
---- to another metric space.
--closestOn :: Metric b => (a -> b) -> a -> [a] -> Maybe a
---- Will not swap elements whose distance is less than d, even if their
---- order implies they should be swapped.
--metricBubbleSort :: (Metric a, Ord a) => Double -> [a] -> [a]
---- Similar to the above, but uses a function to extract the value used for sorting.
--metricBubbleSortOn :: (Metric b, Ord b) => (a -> b) -> Double -> [a] -> [a]
--
---- Bonus (10 points).
--clusters :: Metric a => [a] -> [[a]]
