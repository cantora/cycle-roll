module Data.CycleRoll.LCP where

import Prelude hiding (
  null, head, tail, length, drop
  )
import Data.Vector.Unboxed hiding (find)
import qualified Data.Vector.Algorithms.Intro as ISort
import qualified Control.Monad.ST as ST

find :: (Eq a, Unbox a, Num b) => Vector a -> Vector a -> b
find v w
  | null v            = 0
  | null w            = 0
  | head v == head w  = 1 + find (tail v) (tail w)
  | otherwise         = 0


array :: (Unbox a, Eq a) => Vector a -> Vector Int -> Vector Int
array v suf_arr
  | length suf_arr <= 1     = fromList []
  | otherwise               = next_lcp `cons` array v (tail suf_arr)
  where
    suffix idx 
      | idx == 0   = v
      | otherwise  = drop idx v
    next_lcp = find (suffix (suf_arr!0)) (suffix (suf_arr!1))

sort_array :: Vector Int -> Vector Int
sort_array v = ST.runST $ do
  w <- thaw v
  ISort.sort w
  unsafeFreeze w

  