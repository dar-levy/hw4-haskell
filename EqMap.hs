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
member :: Eq k => k -> EqMap k v -> Bool
insert :: Eq k => k -> v -> EqMap k v -> EqMap k v
remove :: Eq k => k -> EqMap k v -> EqMap k v
lookup :: Eq k => k -> EqMap k v -> Maybe v
assocs :: EqMap k v -> [(k, v)]

instance (Eq k, Eq v) => Eq (EqMap k v)
instance (Show k, Show v) => Show (EqMap k v)
instance Eq k => Semigroup (EqMap k v)
instance Eq k => Monoid (EqMap k v)
newtype CombiningMap k v = CombiningMap {getCombiningMap :: EqMap k v}
instance (Eq k, Semigroup v) => Semigroup (CombiningMap k v)
instance (Eq k, Semigroup v) => Monoid (CombiningMap k v)
