module Data.CycleRoll.LCPTest (
  group
  ) where

import Prelude hiding (
  length, (++), drop, null, head, tail
  )
import qualified Data.CycleRoll.LCP as LCP
import Data.Vector
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import qualified Data.SuffixArray as SA
import Data.Ix
import qualified Data.List as List

group = 
  testGroup "LCP" [
    find_group,
    array_group
    ]

instance (Arbitrary a) => Arbitrary (Vector a) where
  arbitrary    = fmap fromList arbitrary

find_group = 
  testGroup "find" [
    testProperty      "duplicate argument"          prop_dup_arg,
    testProperty      "empty right arg"             prop_empty1,
    testProperty      "empty left arg"              prop_empty2,
    testProperty      "prefix 1"                    prop_prefix1,
    testProperty      "prefix 2"                    prop_prefix2
    ]
  where
    prop_dup_arg :: Vector Char -> Bool
    prop_dup_arg s = (LCP.find s s) == length s

    prop_empty1 :: Vector Char -> Bool
    prop_empty1 s = 0 == LCP.find s empty

    prop_empty2 :: Vector Char -> Bool
    prop_empty2 s = 0 == LCP.find empty s

    prop_prefix1 :: Vector Char -> Vector Char -> Bool
    prop_prefix1 s1 s2 = (LCP.find s1 (s1 ++ s2)) == length s1

    prop_prefix2 :: Vector Char -> Vector Char -> Bool
    prop_prefix2 s1 s2 = (LCP.find (s1 ++ s2) s1) == length s1


array_group =
  testGroup "array" [
    testProperty      "lcp array length"          prop_len,
    testProperty      "lcps are correct"          prop_lcp
    ]
  where
    prop_len :: NonEmptyList Char -> Bool
    prop_len (NonEmpty xs) =
      lcp_arr_len == length arr - 1
      where
        sa@(SA.SuffixArray _ arr)  = SA.fromList xs
        lcp_arr_len                = length $ LCP.array sa

    prop_lcp :: NonEmptyList Char -> NonEmptyList Char -> Bool
    prop_lcp (NonEmpty xs1) (NonEmpty xs2) =
	  check lcparr 0
      where
        sa@(SA.SuffixArray str arr) = SA.fromList $ xs1 List.++ xs2 -- ensures (length lcparr) > 0
        lcparr                      = LCP.array sa
        check la n 
          | null la   = True
          | otherwise = ((LCP.find s1 s2) == head la) && check (tail la) (n+1)
          where 
            s1 = drop (arr!n) str
            s2 = drop (arr!(n+1)) str
