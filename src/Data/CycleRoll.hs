module Data.CycleRoll (
  posDiffs
  ) where

--import qualified Data.CycleRoll.LCP as LCP
--import qualified Data.CycleRoll.SuffixArray as SA

import qualified Data.Vector.Unboxed as UV

posDiffs :: UV.Vector Int -> UV.Vector (Int,Int) -> UV.Vector Int

posDiffs suf_arr lcps =
  UV.map pos_dif lcps 
  where
    pos idx = suf_arr UV.! idx
    pos_dif (_, idx) = abs $ (pos idx) - (pos $ idx+1)
  
--lcpGroups :: (UV.Unbox a) => CRVector a -> [(Int, [UV.Vector a])]
--lcpGroups data_v sorted_lcps suf_arr =
--  where
