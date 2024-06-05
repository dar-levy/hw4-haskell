{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW4.hs EqSet.hs EqMap.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4 where

--import Data.Char (chr, ord)
--import Data.Either
--import Data.List
--import Data.Maybe
--import Data.Semigroup (Arg (..))
--import EqMap (EqMap)
--import EqMap qualified
--import EqSet (EqSet)
--import EqSet qualified

import Data.Char (chr, ord)
import Data.Either
import Data.List (unfoldr)
import Data.Maybe (fromJust, isJust)
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

---- Instance for (a, b)
--instance (Serializable a, Serializable b) => Serializable (a, b) where
--  serialize (x, y) = serialize x ++ serialize y
--  deserialize xs =
--    let (xRest, xs') = splitAt (length (serialize (undefined :: a))) xs
--        (yRest, ys') = splitAt (length (serialize (undefined :: b))) xs'
--    in (deserialize xRest, deserialize yRest)
--
---- Instance for Either a b
--instance (Serializable a, Serializable b) => Serializable (Either a b) where
--  serialize (Left x) = 0 : serialize x
--  serialize (Right y) = 1 : serialize y
--  deserialize (0:xs) = Left (deserialize xs)
--  deserialize (1:xs) = Right (deserialize xs)
--  deserialize _ = error "Invalid input for Either deserialization"
--
---- Instance for [a]
--instance Serializable a => Serializable [a] where
--  serialize [] = [-1]
--  serialize (x:xs) = 0 : serialize x ++ serialize xs
--  deserialize (-1:_) = []
--  deserialize (0:xs) = deserialize' xs
--    where
--      deserialize' [] = []
--      deserialize' ys = let (xRest, ys') = splitAt (length (serialize (undefined :: a))) ys
--                        in deserialize xRest : deserialize ys'
--
---- Rename newtype wrappers to avoid ambiguity
--newtype SerializableEqSet a = SerializableEqSet (ES.EqSet a) deriving (Show, Eq)
--
--instance (Serializable a, Ord a) => Serializable (SerializableEqSet a) where
--  serialize (SerializableEqSet s) = serialize (ES.elems s)
--  deserialize xs = let deserializedList = deserialize xs :: [a]
--                   in SerializableEqSet (foldr ES.insert ES.empty deserializedList)
--
--newtype SerializableEqMap k v = SerializableEqMap (EM.EqMap k v) deriving (Show, Eq)
--
--instance (Serializable k, Ord k, Serializable v) => Serializable (SerializableEqMap k v) where
--  serialize (SerializableEqMap m) = serialize (EM.assocs m)
--  deserialize xs = let deserializedList = deserialize xs :: [(k, v)]
--                   in SerializableEqMap (foldr (uncurry EM.insert) EM.empty deserializedList)

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
