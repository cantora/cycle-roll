module Data.CycleRoll (
  suffixes
  ) where

import qualified Data.CycleRoll.SuffixArray as SA

import qualified Data.Vector.Unboxed as UV

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



