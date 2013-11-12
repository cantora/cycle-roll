module Data.CycleRoll.Test (
  group
  ) where

import qualified Data.CycleRoll.Internal.SuffixArray as SA

import Data.Vector.Unboxed
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

group = 
  testGroup "CycleRoll" [
    basic_group
    ]

basic_group = 
  testGroup "basic" [
    testCase        "suffix array"      suffix_array
    ]
  where
    input   = fromList "ababcabab\0"
    sa      = SA.make input

    suffix_array = (fromList [9,7,5,0,2,8,6,1,3,4]) @=? sa
