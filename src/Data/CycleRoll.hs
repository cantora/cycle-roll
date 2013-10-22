module Data.CycleRoll (
  posDiffs,
  cycleElements,
  SA.make,
  LCP.array,
  UV.fromList,
  suffixes,
  lcpGroups,
  sorted  
  ) where

import qualified Data.CycleRoll.LCP as LCP
import qualified Data.CycleRoll.SuffixArray as SA

import qualified Data.Vector.Unboxed as UV
import qualified Data.Map as Map

lcpGroups = LCP.groups
sorted = LCP.sorted

posDiffs :: UV.Vector Int -> UV.Vector (Int,Int) -> UV.Vector Int
posDiffs suf_arr lcps =
  UV.map pos_dif lcps 
  where
    pos idx = suf_arr UV.! idx
    pos_dif (_, idx) = abs $ (pos idx) - (pos $ idx+1)

  --UV.toList $ UV.map (SA.entry' data_v) sa
suffixes :: UV.Vector Char -> UV.Vector Int -> [[Char]]
suffixes data_v suf_arr =
  process suf_arr
  where
    process sa 
      | sa == UV.empty   = []
      | otherwise        = ((show d_idx) ++ "\t\t" ++ sufx) : (process $ UV.tail sa)
      where
        d_idx = UV.head sa
        sufx  = UV.toList $ SA.entry' data_v d_idx


cycleElements :: (UV.Unbox a) => 
  UV.Vector a -> UV.Vector Int -> UV.Vector (Int,Int) -> [([a],Int)]
cycleElements data_v suf_arr lcps =
  UV.foldl add_element [] lcps
  where
    pos idx     = suf_arr UV.! idx
    pos_dif idx = abs $ (pos idx) - (pos $ idx+1)
    min_pos idx = min (pos idx) (pos $ idx+1)

    add_element els (lcp, idx)
      | lcp == pos_dif idx    = new_el:els
      | otherwise             = els
      where
        prefix = SA.prefix data_v suf_arr idx lcp
        new_el = (UV.toList prefix, min_pos idx)

