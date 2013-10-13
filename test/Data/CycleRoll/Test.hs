module Data.CycleRoll.Test (
  group
  ) where

import qualified Data.CycleRoll.LCP as LCP
import qualified Data.CycleRoll.SuffixArray as SA
import qualified Data.CycleRoll as CR

import Data.Vector.Unboxed
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

group = 
  testGroup "CycleRoll" [
    example_group
    ]

example_group = 
  testGroup "example" [
    testCase        "suffix array"      suffix_array,
    testCase        "lcp array"         lcp_array,
    testCase        "sorted lcp"        sorted_lcp,
    testCase        "pos diffs"         pos_diffs
    ]
  where
    input   = fromList "ababcabab\0"
    sa      = SA.make input
    lcp_arr = LCP.array input sa
    sorted  = LCP.sorted lcp_arr
    pdiffs  = CR.posDiffs sa sorted

    suffix_array = (fromList [9,7,5,0,2,8,6,1,3,4]) @=? sa

    lcp_array = 
      expected @=? lcp_arr
      where
        expected = fromList [(0,0),(2,1),(4,2),(2,3),(0,4),(1,5),(3,6),(1,7),(0,8)]

    sorted_lcp = 
      expected @=? sorted
      where
        expected = fromList [(4,2),(3,6),(2,1),(2,3),(1,5),(1,7),(0,0),(0,4),(0,8)]

    pos_diffs = fromList [5,5,2,2,2,2,2,6,1] @=? pdiffs
