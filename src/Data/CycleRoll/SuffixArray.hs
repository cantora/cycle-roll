module Data.CycleRoll.SuffixArray (
  make,
  entry,
  entry',
  prefix
  ) where

import qualified Data.SuffixArray as SA
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import Data.Ix

--fromList :: (Ix a, Bounded a, UV.Unbox a) => [a] -> UV.Vector Int
--fromList els = make $ UV.fromList els

make :: (Ix a, Bounded a, UV.Unbox a) => UV.Vector a -> UV.Vector Int
make v = 
  GV.convert $ get_sa $ SA.suffixArray $ GV.convert v
  where
    get_sa (SA.SuffixArray _ sa) = sa

entry :: (UV.Unbox a) => UV.Vector a -> UV.Vector Int -> Int -> UV.Vector a
entry v sa idx = entry' v (sa UV.! idx)

entry' :: (UV.Unbox a) => UV.Vector a -> Int -> UV.Vector a
entry' v idx = UV.drop idx v

prefix :: (UV.Unbox a) => 
  UV.Vector a -> UV.Vector Int -> Int -> Int -> UV.Vector a
prefix v sa idx n = UV.slice (sa UV.! idx) n v