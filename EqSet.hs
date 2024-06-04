{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module EqSet (
  EqSet,
  empty,
  EqSet.insert, -- To avoid name clash with Data.List.insert
  member,
  remove,
  elems,
) where

import Data.Either
import Data.List
import Data.Maybe

newtype EqSet a = EqSet {getSet :: [a]}

empty :: EqSet a
empty = EqSet []

member :: Eq a => a -> EqSet a -> Bool
member _ (EqSet []) = False
member y (EqSet (x:xs)) = if x == y then True else member y (EqSet xs)

insert :: Eq a => a -> EqSet a -> EqSet a
insert x (EqSet xs) = if member x (EqSet xs) then EqSet xs else EqSet (x:xs)

remove :: Eq a => a -> EqSet a -> EqSet a
remove _ (EqSet []) = EqSet []
remove y (EqSet xs) = EqSet (removeAux y xs)
  where
    removeAux :: Eq a => a -> [a] -> [a]
    removeAux _ [] = []
    removeAux a (b:bs)
      | a == b = bs
      | otherwise = b : (removeAux a bs)

elems :: EqSet a -> [a]
elems = getSet

instance Eq a => Eq (EqSet a) where
  EqSet xs == EqSet ys = eqAux (EqSet xs) (EqSet ys)
   where
    eqAux :: Eq a => EqSet a -> EqSet a -> Bool
    eqAux (EqSet []) _ = True
    eqAux (EqSet (a:as)) (EqSet bs) = if member a (EqSet bs) then eqAux (EqSet as) (EqSet bs) else False

instance Show a => Show (EqSet a) where
  show :: Show a => EqSet a -> String
  show (EqSet xs) = "{" ++ (showAux (EqSet xs)) ++ "}"
   where
    showAux :: Show a => EqSet a -> String
    showAux (EqSet []) = ""
    showAux (EqSet [a]) = show a
    showAux (EqSet (a:as)) = show a ++ ", " ++ showAux (EqSet as)

instance Eq a => Semigroup (EqSet a) where
  (<>) :: Eq a => EqSet a -> EqSet a -> EqSet a
  (EqSet xs) <> (EqSet ys) = unionAux (EqSet xs) (EqSet ys)
   where
    unionAux :: Eq a => EqSet a -> EqSet a -> EqSet a
    unionAux (EqSet []) (EqSet bs) = EqSet bs
    unionAux (EqSet (a:as)) (EqSet bs) = unionAux (EqSet as) (EqSet.insert a (EqSet bs))

instance Eq a => Monoid (EqSet a) where
  mempty :: Eq a => EqSet a
  mempty = empty
