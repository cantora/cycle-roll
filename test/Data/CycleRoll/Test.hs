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
import qualified Data.List as List
import Debug.Trace

group = 
  testGroup "CycleRoll" [
    basic_group,
    rSeqNode_group
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
        max_amt <- choose (2, n)
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



rSeqNode_group = 
  testGroup "rSeqNode" [
    testProperty "len"         prop_len,
    testProperty "fold_eq_len" prop_fold_eq_len,
    testProperty "fold_offset" prop_fold_offset,
    testCase     "fold_test1"  fold_test1
    ]
  where
    prop_len :: RSeqNodeWSz -> Bool
    prop_len (RSeqNodeWSz sz rsnode) = 
      sz == (CR.rSeqNodeLength rsnode)

    prop_fold_eq_len :: RSeqNodeWSz -> Bool
    prop_fold_eq_len (RSeqNodeWSz sz rsnode) = 
      let
        fn _ lvs leaf = leaf:lvs
        folded = CR.foldRSeqNode fn 0 [] rsnode
      in (CR.rSeqNodeLength rsnode) == fst folded

    prop_fold_offset :: Int -> RSeqNodeWSz -> Bool
    prop_fold_offset start_off (RSeqNodeWSz sz rsnode) = 
      let
        fn _ lvs leaf = leaf:lvs
        folded = fst $ CR.foldRSeqNode fn start_off [] rsnode
      in (start_off + (CR.rSeqNodeLength rsnode)) == folded

    fold_test1 =
      let 
        mp = [
          0, 4, 4+4, (8+4)+8,
          4+5*(8*2 + 6*3),
          174 + 3,
          177 + 5*4,
          197 + 3*2
          ]

        rseq = 
          CR.RSeqNode 0 [
            CR.RSeqLeaf 2 1,     -- 0
            CR.RSeqNode 4 [
              CR.RSeqNode 1 [
                CR.RSeqLeaf 4 0, -- 4
                CR.RSeqLeaf 1 3  -- 8
                ],
              CR.RSeqLeaf 6 2    -- (8+4)+8
              ],
            CR.RSeqLeaf 3 0,     -- 4+5*(8*2 + 6*3)
            CR.RSeqNode 1 [
              CR.RSeqLeaf 5 3,   -- 174+3
              CR.RSeqNode 3 [    
                CR.RSeqLeaf 3 1, -- 177+5*4
                CR.RSeqLeaf 2 3  -- 197+3*2
                ]
              ]
            ]            

        fn off (idx, bl) lf = trace (show (off, lf)) $ (idx+1, bl && (mp List.!! idx) == off)
      in (8, True) @=? (snd $ CR.foldRSeqNode fn 0 (0, True) rseq)

{-
-}
