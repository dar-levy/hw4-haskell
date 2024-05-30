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
empty = undefined

member :: Eq a => a -> EqSet a -> Bool
member = undefined

insert :: Eq a => a -> EqSet a -> EqSet a
insert = undefined

remove :: Eq a => a -> EqSet a -> EqSet a
remove = undefined

elems :: EqSet a -> [a]
elems = undefined

instance Eq a => Eq (EqSet a)
instance Show a => Show (EqSet a)
instance Eq a => Semigroup (EqSet a)
instance Eq a => Monoid (EqSet a)
