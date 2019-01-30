module MyMap where

import Data.Data (Data)
import qualified RedBlackTree as RBT

data MapEntry k a = MapEntry k a deriving (Data, Show)

-- this seems dangerous but it's easy so what could go wrong?
instance (Eq k) => Eq (MapEntry k a) where
  (MapEntry k1 _) == (MapEntry k2 _) = k1 == k2

instance (Ord k) => Ord (MapEntry k a) where
  compare (MapEntry k1 _) (MapEntry k2 _) = compare k1 k2

data Map k a = MapC (RBT.RBTree (MapEntry k a)) deriving Data

insert :: (Ord k) => k -> a -> Map k a -> Map k a
insert key val (MapC map) = MapC $ RBT.insertNoDupes map (MapEntry key val)

empty = MapC RBT.NilNode

size :: Map k a -> Int
size (MapC map) = RBT.size map

member :: Ord k => k -> Map k a -> Bool
member k (MapC map) = RBT.contains map (MapEntry k undefined)

entryToTuple :: MapEntry k a -> (k, a)
entryToTuple (MapEntry key val) = (key, val)

toList :: Map k a -> [(k, a)]
toList (MapC m) = map entryToTuple $ RBT.treeToList m

fromList ::  (Ord k) => [(k, a)] -> Map k a
fromList tuples = let
  insertTuple (k, v) m = insert k v m
  tupleToEntry x y = MapEntry x y
  in foldr insertTuple empty tuples

union :: (Ord k) => Map k a -> Map k a -> Map k a
union (MapC mWinner) (MapC mLoser) =
  MapC $ foldr (flip RBT.insertNoDupes) mLoser (RBT.treeToList mWinner)

unions :: (Ord k) => [Map k a] -> Map k a
unions maps = foldr union empty (reverse maps)

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault d k (MapC m) = case (RBT.lookupElem m (MapEntry k undefined)) of
  Just (MapEntry _ a) -> a
  Nothing             -> d

(!) :: Ord k => Map k a -> k -> a
m!k = findWithDefault (error "no such key") k m

findOrCrash k m = m!k
