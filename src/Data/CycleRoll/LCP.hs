module Data.CycleRoll.LCP where

import Prelude hiding (
  null, head, tail, length
  )

import qualified Data.CycleRoll.SuffixArray as SA

import Data.Vector.Unboxed hiding (find)
import qualified Data.Vector.Algorithms.Intro as ISort
import qualified Control.Monad.ST as ST
import Data.Monoid

find :: (Eq a, Unbox a, Num b) => Vector a -> Vector a -> b
find v w
  | null v            = 0
  | null w            = 0
  | head v == head w  = 1 + find (tail v) (tail w)
  | otherwise         = 0


array :: (Unbox a, Eq a) => Vector a -> Vector Int -> Vector (Int,Int)
array v suf_arr =
  array' suf_arr 0
  where
    array' sa n
      | length sa <= 1     = fromList []
      | otherwise          = (next_lcp, n) `cons` array' (tail sa) (n+1)
      where
        suffix idx = SA.entry idx v sa
        next_lcp = find (suffix 0) (suffix 1)

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
