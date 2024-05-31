{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module EqMap (
  EqMap,
  CombiningMap (..),
  empty,
  EqMap.insert, -- To avoid name clash with Data.List.insert
  member,
  remove,
  EqMap.lookup, -- To avoid name clash with Prelude.lookup
  assocs
) where

import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup (Arg (..))
import EqSet (EqSet)
import EqSet qualified

data EqMap k v -- Complete me!

empty :: EqMap k v
empty = undefined

member :: Eq k => k -> EqMap k v -> Bool
member = undefined

insert :: Eq k => k -> v -> EqMap k v -> EqMap k v
insert = undefined

remove :: Eq k => k -> EqMap k v -> EqMap k v
remove = undefined

lookup :: Eq k => k -> EqMap k v -> Maybe v
lookup = undefined

assocs :: EqMap k v -> [(k, v)]
assocs = undefined

instance (Eq k, Eq v) => Eq (EqMap k v) where
  (==) = undefined

instance (Show k, Show v) => Show (EqMap k v) where
  show = undefined

instance Eq k => Semigroup (EqMap k v) where
  (<>) = undefined

instance Eq k => Monoid (EqMap k v) where
  mempty = undefined
  mappend = (<>)

newtype CombiningMap k v = CombiningMap { getCombiningMap :: EqMap k v }

instance (Eq k, Semigroup v) => Semigroup (CombiningMap k v) where
  (<>) = undefined

instance (Eq k, Semigroup v) => Monoid (CombiningMap k v) where
  mempty = undefined
  mappend = (<>)
