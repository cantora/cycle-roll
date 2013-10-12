module Data.CycleRoll.SuffixArrayTest (
  group
  ) where

import qualified Data.CycleRoll.SuffixArray as SA
import Data.Vector.Unboxed
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit

group = 
  testGroup "SuffixArray" [
    sanity_group
    ]

sanity_group = 
  testGroup "sanity" [
    testCase     "make sanity"    make_sanity
    ]
  where
    make_sanity =
      (fromList [5,3,1,0,4,2]) @=? (SA.make $ fromList "banana")
