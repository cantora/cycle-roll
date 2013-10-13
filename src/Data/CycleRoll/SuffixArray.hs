module Data.CycleRoll.SuffixArray (
  make,
  entry
  ) where

import qualified Data.SuffixArray as SA
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import Data.Ix

make :: (Ix a, Bounded a, UV.Unbox a) => UV.Vector a -> UV.Vector Int
make v = 
  GV.convert $ get_sa $ SA.suffixArray $ GV.convert v
  where
    get_sa (SA.SuffixArray _ sa) = sa

entry :: (UV.Unbox a) => Int -> UV.Vector a -> UV.Vector Int -> UV.Vector a
entry idx v sa = UV.drop (sa UV.! idx) v

