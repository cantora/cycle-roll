module Data.CycleRoll.LCP where

import Prelude hiding (
  null, head, tail, length, drop
  )
import Data.Vector hiding (find)
import qualified Data.SuffixArray as SA

find :: (Eq a, Num b) => Vector a -> Vector a -> b
find v w
  | null v            = 0
  | null w            = 0
  | head v == head w  = 1 + find (tail v) (tail w)
  | otherwise         = 0

array :: (Eq a) => SA.SuffixArray a -> Vector Int
array (SA.SuffixArray str arr) = array' str arr 

array' :: (Eq a) => Vector a -> Vector Int -> Vector Int
array' str arr
  | length arr <= 1     = fromList []
  | otherwise           = next_lcp `cons` array' str (tail arr)
  where
    suffix idx 
      | idx == 0   = str
      | otherwise  = drop idx str
    next_lcp = find (suffix (arr!0)) (suffix (arr!1))

