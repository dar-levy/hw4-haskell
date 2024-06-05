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

-- Section 3: Metric
infinity :: Double
infinity = 1 / 0

class Eq a => Metric a where
  distance :: a -> a -> Double

instance Metric Double where
  distance x y = abs (x - y)

instance Metric Int where
  distance x y = abs (fromIntegral x - fromIntegral y)
  --distance x y = distance (fromIntegral x) (fromIntegral y)

instance Metric Char where
  distance c1 c2 = distance (ord c1) (ord c2)

-- Euclidean distance
instance (Metric a, Metric b) => Metric (a, b) where
  --distance (x1, y1) (x2, y2) = ((distance x1 x2) ** 2 + (distance y1 y2) ** 2) ** 0.5
  distance (x1, y1) (x2, y2) = sqrt ((distance x1 x2) ** 2 + (distance y1 y2) ** 2)

data ManhattanTuple a b = ManhattanTuple a b deriving Eq
instance (Metric a, Metric b) => Metric (ManhattanTuple a b) where
  distance (ManhattanTuple a1 b1) (ManhattanTuple a2 b2) = (distance a1 a2) + (distance b1 b2)

-- Just and Nothing have distance of infinity.
-- Two Justs measure the distance between the two values.
instance Metric a => Metric (Maybe a) where

  distance (Just x) (Just y) = distance x y
  distance Nothing (Just _) = infinity
  distance (Just _) Nothing = infinity
  distance Nothing Nothing = 0

-- Left and Right have a distance of infinity.
-- Same constructores measure the distance between the two values.
instance (Metric a, Metric b) => Metric (Either a b) where
  distance (Left x) (Left y) = distance x y
  distance (Right x) (Right y) = distance x y
  distance (Left _) (Right _) = infinity
  distance (Right _) (Left _) = infinity

-- Lists of different sizes have distance of infinity.
-- Euclidean distance.
instance Metric a => Metric [a] where
  distance as bs = (distanceAux as bs) ** 0.5
   where
   distanceAux :: Metric a => [a] -> [a] -> Double
   distanceAux [x] [y] = (distance x y) ** 2
   distanceAux (x:xs) (y:ys) = ((distance x y) ** 2) + (distanceAux xs ys)
   distanceAux [] _ = infinity
   distanceAux _ [] = infinity

newtype ManhattanList a = ManhattanList [a] deriving Eq
instance Metric a => Metric (ManhattanList a) where
  
  distance (ManhattanList [a]) (ManhattanList [b]) = distance a b
  distance (ManhattanList (a:as)) (ManhattanList (b:bs)) = (distance a b) + (distance (ManhattanList as) (ManhattanList bs))
  distance (ManhattanList _) (ManhattanList []) = infinity
  distance (ManhattanList []) (ManhattanList _) = infinity

-- Returns the element with the shortest distance to the input.
-- If there are no numbers whose distance is less than infinity, return Nothing.
closest :: Metric a => a -> [a] -> Maybe a
closest = closestOn id
      
-- Similar to the above, but uses a function move the element
-- to another metric space.
closestOn :: Metric b => (a -> b) -> a -> [a] -> Maybe a
closestOn f' a as = closestOnAux f' a as infinity Nothing
  where
    --d = closestAux a as infinity Nothing
    closestOnAux :: Metric b => (a -> b) -> a -> [a] -> Double -> Maybe a -> Maybe a
    closestOnAux _ _ [] _ min_elem = min_elem
    closestOnAux f x (y:ys) min_dist min_elem = if dist < min_dist
      then closestOnAux f x ys dist (Just y)
      else closestOnAux f x ys min_dist min_elem
      where
      dist = distance (f x) (f y)


-- Will not swap elements whose distance is less than d, even if their
-- order implies they should be swapped.
metricBubbleSort :: (Metric a, Ord a) => Double -> [a] -> [a]
metricBubbleSort = metricBubbleSortOn id

-- Similar to the above, but uses a function to extract the value used for sorting.
metricBubbleSortOn :: (Metric b, Ord b) => (a -> b) -> Double -> [a] -> [a]
metricBubbleSortOn f' d' as = metricBubbleSortOnAux f' d' 0 (Data.List.length as) as
 where
  metricBubbleSortOnAux :: (Metric b, Ord b) => (a -> b) -> Double -> Int -> Int -> [a] -> [a]
  metricBubbleSortOnAux f d num_iter len xs = if num_iter < len
    then metricBubbleSortOnAux f d  (num_iter+1) len (iteration f d xs)
    else xs

  iteration :: (Metric b, Ord b) => (a -> b) -> Double -> [a] -> [a] 
  iteration _ _ [] = []
  iteration _ _ [x] = [x]
  iteration f d (x:y:xys) = if condition then y : (iteration f d (x:xys)) else x : (iteration f d (y:xys))
   where
    condition = (fx > fy) && ((distance fx fy) > d)
    fx = f x
    fy = f y

-- Bonus (10 points).
clusters :: Metric a => [a] -> [[a]]
clusters [] = []
clusters (a:as) = reachables' : (clusters unreachables')
 where
  split :: Metric a => a -> [a] -> ([a], [a]) -> ([a], [a])
  split _ [] (reachables, unreachables) = (reachables, unreachables)
  split t (x:xs) (reachables, unreachables) = if (distance x t) < infinity
    then split t xs (x:reachables, unreachables)
    else split t xs (reachables, x:unreachables)

  (reachables', unreachables') = (split a (a:as) ([], []))

