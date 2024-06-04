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

data EqMap k v = EqMap (EqSet k) [Arg k v] -- Complete me!

empty :: EqMap k v
empty = EqMap EqSet.empty []

member :: Eq k => k -> EqMap k v -> Bool
member key (EqMap keys _) = EqSet.member key keys

insert :: Eq k => k -> v -> EqMap k v -> EqMap k v
insert key val (EqMap keys key_vals) = if member key (EqMap keys key_vals)
  then EqMap.insert key val (remove key (EqMap keys key_vals))
  else EqMap (EqSet.insert key keys) ((Arg key val) : key_vals)

remove :: Eq k => k -> EqMap k v -> EqMap k v
--remove _ (EqMap _ []) = EqMap.empty
remove key (EqMap keys key_vals) = EqMap (EqSet.remove key keys) (removeAux key key_vals)
  where
    removeAux :: Eq k => k -> [Arg k v] -> [Arg k v]
    removeAux _ [] = []
    removeAux key1 ((Arg k v):kvs) -- key1 is the name to not shadow the already defined variable named key
      | key1 == k = kvs
      | otherwise = (Arg k v) : (removeAux key1 kvs)

lookup :: Eq k => k -> EqMap k v -> Maybe v -- test
lookup _ (EqMap _ []) = Nothing
lookup key (EqMap keys ((Arg k v):key_vals))
  | key == k = (Just v)
  | otherwise = EqMap.lookup key (EqMap keys key_vals)

assocs :: EqMap k v -> [(k, v)] -- test
assocs (EqMap _ []) = []
assocs (EqMap keys ((Arg k v):key_vals)) = (k, v) : assocs (EqMap keys key_vals)

instance (Eq k, Eq v) => Eq (EqMap k v) where -- test
  (==) :: (Eq k, Eq v) => EqMap k v -> EqMap k v -> Bool
  (EqMap kmap1 vmap1) == (EqMap kmap2 vmap2) = eqAux (EqMap kmap1 vmap1) (EqMap kmap2 vmap2)
   where
    eqAux :: (Eq k, Eq v) => EqMap k v -> EqMap k v -> Bool
    eqAux (EqMap _ []) (EqMap _ []) = True
    eqAux (EqMap _ ((Arg _ _):_)) (EqMap _ []) = False
    eqAux (EqMap _ []) (EqMap _ ((Arg _ _):_)) = False
    eqAux (EqMap keys1 ((Arg k1 _):key_vals1)) (EqMap keys2 ((Arg k2 _):key_vals2)) = k1 == k2 &&
      eqAux (EqMap keys1 key_vals1) (EqMap keys2 key_vals2)


instance (Show k, Show v) => Show (EqMap k v) where
  show :: (Show k, Show v) => EqMap k v -> String
  show (EqMap _ v) = "{" ++ showAux v ++ "}"
    where
    showAux :: (Show k, Show v) => [Arg k v] -> String
    showAux [] = ""
    showAux [Arg key val] = show key ++ "->" ++ show val
    showAux ((Arg key val):keys_vals) = show key ++ "->" ++ show val ++ "," ++ showAux keys_vals

instance Eq k => Semigroup (EqMap k v) where -- test
  (<>) :: Eq k => EqMap k v -> EqMap k v -> EqMap k v
  (EqMap kmap1 vmap1) <> (EqMap kmap2 vmap2) = unionAux (EqMap kmap1 vmap1) (EqMap kmap2 vmap2)
   where
    unionAux :: Eq k => EqMap k v -> EqMap k v -> EqMap k v
    unionAux (EqMap keys1 key_vals1) (EqMap _ []) = EqMap keys1 key_vals1
    unionAux (EqMap keys1 key_vals1) (EqMap keys2 ((Arg k2 v2):key_vals2)) = 
      unionAux (EqMap.insert k2 v2 (EqMap keys1 key_vals1)) (EqMap keys2 key_vals2)

instance Eq k => Monoid (EqMap k v) where
  mempty :: Eq k => EqMap k v
  mempty = empty
  
newtype CombiningMap k v = CombiningMap {getCombiningMap :: EqMap k v} -- test
instance (Eq k, Semigroup v) => Semigroup (CombiningMap k v) where
  (<>) :: (Eq k, Semigroup v) => CombiningMap k v -> CombiningMap k v -> CombiningMap k v
  -- (CombiningMap (EqMap k1 ((Arg key val):v1))) <> (CombiningMap (EqMap k2 v2))
  --  | member key (EqMap k2 v2) = (val <> EqMap.lookup key (EqMap k2 v2))
  --  | otherwise = undefined
  --  where

    --unionAux :: (Eq k, Semigroup v) => CombiningMap k v -> CombiningMap k v -> CombiningMap k v
    --unionAux (CombiningMap (EqMap k1 ((Arg key val):v1))) (CombiningMap (EqMap k2 v2)) = if member key (EqMap k2 v2)
      --then unionAux (EqMap.insert key (val <> (lookup key (EqMap k2 v2))) (EqMap k2 v2))
      --else 
  -- (CombiningMap (EqMap k1 v1)) <> (CombiningMap (EqMap k2 v2)) = if member k1 (EqMap k2 v2)
  --   then EqMap.insert k1 (v1 <> v2) (EqMap k2 v2)
  (CombiningMap (EqMap _ _)) <> (CombiningMap (EqMap _ _)) = undefined

instance (Eq k, Semigroup v) => Monoid (CombiningMap k v) where
  mempty :: (Eq k, Semigroup v) => CombiningMap k v
  mempty = CombiningMap empty
