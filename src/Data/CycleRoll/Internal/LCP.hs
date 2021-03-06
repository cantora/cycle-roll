module Data.CycleRoll.Internal.LCP where

import Prelude hiding (
  null, head, tail, length, last, sum
  )

import qualified Data.CycleRoll.Internal.SuffixArray as SA

import Data.Vector.Unboxed hiding (find, sum)
import qualified Data.Vector.Primitive as PV
import qualified Data.List as List
import qualified Data.Heap as Heap
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable
import qualified Data.RangeMin as RangeMin

find :: (Eq a, Unbox a, Num b) => Vector a -> Vector a -> b
find v w
  | null v            = 0
  | null w            = 0
  | head v == head w  = 1 + find (tail v) (tail w)
  | otherwise         = 0


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

rmq :: Vector Int -> (Int -> Int -> Int -> Bool)
rmq lcparr = 
  ret_fn $ RangeMin.intRangeMin pv_lcparr
  where
    pv_lcparr :: PV.Vector Int
    pv_lcparr = PV.convert lcparr
    ret_fn rmq_fn idx1 idx2 min_val
      | idx1 == idx2   = error $ (show idx1) List.++ " = idx1 == idx2 = " List.++ (show idx2)
      | idx1 < idx2    = not . test $ rmq_fn idx1 (idx2-idx1)
      | otherwise      = ret_fn rmq_fn idx2 idx1 min_val
      where
        test i = lcparr!i < min_val
    
data GroupElem = 
  GroupElem { 
    srcIdx :: Int,
    saIdx :: Int
    } deriving (Show, Eq, Ord)

data Group =
  Group {
    groupLCP :: Int,
    groupMembers :: Heap.Heap GroupElem
    } deriving (Eq)

--invert compare for Group so Heap will prioritize higher LCP
instance Ord Group where
  compare a b = (groupLCP b) `compare` (groupLCP a)

instance Show Group where
  show (Group lcp memb) = 
    "  " List.++ (show lcp) List.++ ": " List.++ (Foldable.foldr fn "" memb) List.++ "\n"
    where
      fn (GroupElem a b) sum = "\n    " List.++ (show (a,b)) List.++ sum

groups :: Vector Int -> Vector Int -> Heap.Heap Group
groups sarr lcparr 
  | length lcparr < 1 = Heap.empty
  | otherwise         = Map.foldlWithKey fold_fn Heap.empty lcp_group_map
  where
    fold_fn hp key val = Heap.insert (Group key val) hp

    lcp_group_map = 
      add_ends_to middle
      where
        final_idx = (length sarr)-1

        map_append k v mp
          | k < 1           = mp -- LCP of zero is not a useful LCP group
          | otherwise       = Map.insert k new_el mp
          where
            new_el = Heap.insert v $ Map.findWithDefault Heap.empty k mp

        add_ends_to mp = 
          map_append (lcparr!0) beg $ map_append (lcparr!(final_idx-1)) end mp
          where
            beg = GroupElem (head sarr) 0
            end = GroupElem (last sarr) final_idx

        middle = 
          List.foldl add_element Map.empty [1..(final_idx-1)]
          where
            add_element mp sa_idx = 
              map_append lcp (GroupElem (sarr!sa_idx) sa_idx) mp
              where
                lcp = max (lcparr!(sa_idx-1)) $ lcparr!sa_idx


mergedGroups :: Heap.Heap Group -> [Group]
mergedGroups grp_hp =
  recurse Heap.empty [] grp_hp
  where
    recurse prev_mb base heap 
      | Heap.null heap   = base
      | otherwise   = cur:(recurse merged base $ Heap.deleteMin heap)
      where
        cur_grp   = Heap.minimum heap
        merged    = (groupMembers cur_grp) `Heap.union` prev_mb
        cur       = Group (groupLCP cur_grp) merged
