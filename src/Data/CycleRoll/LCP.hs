module Data.CycleRoll.LCP where

import Prelude hiding (
  null, head, tail, length, last
  )

import qualified Data.CycleRoll.SuffixArray as SA

import Data.Vector.Unboxed hiding (find)
import qualified Data.Vector.Algorithms.Intro as ISort
import qualified Control.Monad.ST as ST
import Data.Monoid
import qualified Data.List as List
import qualified Data.Heap as Heap
import qualified Data.Map as Map

find :: (Eq a, Unbox a, Num b) => Vector a -> Vector a -> b
find v w
  | null v            = 0
  | null w            = 0
  | head v == head w  = 1 + find (tail v) (tail w)
  | otherwise         = 0


{-
array :: (Unbox a, Eq a) => Vector a -> Vector Int -> Vector (Int,Int)
array v suf_arr =
  array' suf_arr 0
  where
    array' sa n
      | length sa <= 1     = fromList []
      | otherwise          = (next_lcp, n) `cons` array' (tail sa) (n+1)
      where
        suffix = SA.entry v sa
        next_lcp = find (suffix 0) (suffix 1)
-}

array :: (Unbox a, Eq a) => Vector a -> Vector Int -> Vector Int
array v suf_arr 
  | sa_len <= 1   = fromList []
  | otherwise     = List.foldr fn empty [0..(sa_len-2)]
  where
    sa_len = length suf_arr
    fn i sum = 
      (find (suffix i) $ suffix $ i+1) `cons` sum
      where
        suffix = SA.entry v suf_arr


sorted :: Vector (Int,Int) -> Vector (Int,Int)
sorted v = 
  ST.runST $ do
    w <- thaw v
    ISort.sortBy compareLCPElement w
    unsafeFreeze w

-- compare the lcp value in descending order and the index value
-- in ascending order
compareLCPElement :: (Int, Int) -> (Int, Int) -> Ordering
compareLCPElement (lcp1, idx1) (lcp2, idx2) = 
  (compare lcp2 lcp1) `mappend` (compare idx1 idx2)


data GroupElem = 
  GroupElem { 
    srcIdx :: Int,
    saIdx :: Int
    } deriving (Show, Eq, Ord)

data Group =
  Group {
    groupLCP :: Int,
    groupMembers :: Heap.Heap GroupElem
    } deriving (Show, Eq)

--invert compare for Group so Heap will prioritize higher LCP
instance Ord Group where
  compare a b = (groupLCP b) `compare` (groupLCP a)

groups :: Vector Int -> Vector Int -> Heap.Heap Group
groups sarr lcparr 
  | length lcparr < 1 = Heap.empty
  | otherwise         = Map.foldlWithKey fold_fn Heap.empty lcpGroupMap
  where
    fold_fn hp key val = Heap.insert (Group key val) hp

    lcpGroupMap = 
      add_ends_to middle
      where
        final_idx = (length sarr)-2

        map_append k v map 
          | k < 1     = map -- LCP of zero is not a useful LCP group
          | otherwise = Map.insert k new_el map
          where
            old_el = Map.findWithDefault Heap.empty k map
            new_el = Heap.insert v old_el

        add_ends_to map = 
          map_append (lcparr!0) beg $ map_append (lcparr!final_idx) end map
          where
            beg = GroupElem (head sarr) 0
            end = GroupElem (last sarr) final_idx

        middle = 
          List.foldl add_element Map.empty [1..(final_idx-1)]
          where
            add_element map sa_idx = 
              map_append lcp (GroupElem (sarr!sa_idx) sa_idx) map
              where
                lcp = max (lcparr!(sa_idx-1)) $ lcparr!sa_idx


mergedGroups :: Vector Int -> Vector Int -> [Group]
mergedGroups sarr lcparr =
  recurse Heap.empty [] $ groups sarr lcparr
  where
    recurse prev_mb base heap 
      | Heap.null heap   = base
      | otherwise   = cur:(recurse merged base $ Heap.deleteMin heap)
      where
        cur_grp   = Heap.minimum heap
        merged    = (groupMembers cur_grp) `Heap.union` prev_mb
        cur       = Group (groupLCP cur_grp) merged
