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
    basic_group
    ]

basic_group = 
  testGroup "example" [
    testCase        "suffix array"      suffix_array
    ]
  where
    input   = fromList "ababcabab\0"
    sa      = SA.make input

    suffix_array = (fromList [9,7,5,0,2,8,6,1,3,4]) @=? sa
