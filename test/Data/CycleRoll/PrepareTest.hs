module Data.CycleRoll.PrepareTest (
  group
  ) where

import Prelude hiding (
  length, (++), null, head, tail
  )
import qualified Data.CycleRoll.Prepare as Prepare
import qualified Data.CycleRoll.SuffixArray as SA
import Test.Utils

import Data.Vector.Unboxed
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Ix
import qualified Data.List as List

group = 
  testGroup "Prepare" [
    basic_group
    ]

testStr :: String
testStr = "put that ghost down and help me carry this clown-bot"

basic_group = 
  testGroup "basic" [
    testCase    "sanity"    sanity
  ]
  where
    sanity =
       (fromList $ testStr List.++ "\x00") @=? Prepare.fromList '\x00' testStr
