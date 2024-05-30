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
member :: Eq a => a -> EqSet a -> Bool
insert :: Eq a => a -> EqSet a -> EqSet a
remove :: Eq a => a -> EqSet a -> EqSet a
elems :: EqSet a -> [a]

instance Eq a => Eq (EqSet a)
instance Show a => Show (EqSet a)
instance Eq a => Semigroup (EqSet a)
instance Eq a => Monoid (EqSet a)
