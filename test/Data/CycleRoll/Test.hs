module Data.CycleRoll.Test (
  group
  ) where

import qualified Data.CycleRoll.LCP as LCP
import qualified Data.CycleRoll.SuffixArray as SA
import qualified Data.CycleRoll as CR

import Data.Vector.Unboxed
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Control.Monad

group = 
  testGroup "CycleRoll" [
    basic_group,
    rSeqNodeLength_group
    ]

basic_group = 
  testGroup "basic" [
    testCase        "suffix array"      suffix_array
    ]
  where
    input   = fromList "ababcabab\0"
    sa      = SA.make input

    suffix_array = (fromList [9,7,5,0,2,8,6,1,3,4]) @=? sa

data RSeqNodeWSz = 
  RSeqNodeWSz Int CR.RSeqNode deriving (Show, Eq, Ord)

instance Arbitrary RSeqNodeWSz where
  arbitrary =
    sized make'
    where
      make' n
        | n < 1     = make' 1
        | otherwise = liftM (RSeqNodeWSz n) $ make n

      make_subs parent_sz max_amt amt total
        | total > parent_sz  = fail "total exceeds parent size!"
        | total == parent_sz = return (total, [])
        | max_amt < amt      = fail "amt exceeds max amt"
        | max_amt == amt     = return (total, [])
        | otherwise          = make_subs'
        where
          recurse sz = 
            make_subs parent_sz max_amt (amt+1) (total+sz)
          make_subs' = do
            sz  <- choose (1, parent_sz-total)
            sub <- make sz
            (accum_sz, subs) <- recurse sz
            return (accum_sz, sub:subs)

      find_rpt n m =
        recurse 0
        where
          recurse x
            | n < m'    = error "must have m <= n"
            | n == m'   = (0, n-m)
            | q <= 1    = (0, n-m)
            | r < 1     = (q-1, x)
            | q >= r    = recurse $ x+1
            | otherwise = recurse $ x + (r `div` q)
            where
              m'     = m+x
              (q, r) = n `quotRem` m'

      make_node n = do
        max_amt <- choose (1, n)
        (used, subs) <- make_subs n max_amt 0 0
        let (rpt, remain) = find_rpt n used
        if (remain > 0)
          then do
            extra <- make remain
            return $ CR.RSeqNode rpt $ extra:subs
          else return $ CR.RSeqNode rpt subs

      make_leaf n = 
        do
          q <- liftM (find_divisor n) $ choose (1, n)
          return $ CR.RSeqLeaf q $ (n `div` q) - 1
        where
          find_divisor n m 
            | m > n           = error "m must be <= n"
            | n `mod` m == 0  = m
            | otherwise       = find_divisor n $ m+1

      make 0 = fail "cant make rseqnode with 0 size"
      make 1 = make_leaf 1
      make n = frequency [(1, make_leaf n), (1, make_node n)]



rSeqNodeLength_group = 
  testGroup "rSeqNodeLength" [
    testProperty     "len"    prop_len
    ]
  where
    prop_len :: RSeqNodeWSz -> Bool
    prop_len (RSeqNodeWSz sz rsnode) = sz == (CR.rSeqNodeLength rsnode)
