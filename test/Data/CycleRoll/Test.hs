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
    rSeqNode_group,
    mergeSubSeq_group
    ]

basic_group = 
  testGroup "basic" [
    testCase        "suffix array"      suffix_array
    ]
  where
    input   = fromList "ababcabab\0"
    sa      = SA.make input

    suffix_array = (fromList [9,7,5,0,2,8,6,1,3,4]) @=? sa

makeRSeqLeaf :: Int -> Gen CR.RSeqNode
makeRSeqLeaf n = 
  do
    q <- liftM (find_divisor n) $ choose (1, n)
    return $ CR.RSeqLeaf q $ (n `div` q) - 1
  where
    find_divisor n m 
      | m > n           = error "m must be <= n"
      | n `mod` m == 0  = m
      | otherwise       = find_divisor n $ m+1

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

      make 0 = fail "cant make rseqnode with 0 size"
      make 1 = makeRSeqLeaf 1
      make n = frequency [(1, makeRSeqLeaf n), (1, make_node n)]


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

        fn off (idx, bl) lf = (idx+1, bl && (mp List.!! idx) == off)
      in (8, True) @=? (snd $ CR.foldRSeqNode fn 0 (0, True) rseq)


data RSeqLeafNode = RSeqLeafNode CR.RSeqNode deriving (Show, Eq, Ord)
instance Arbitrary RSeqLeafNode where
  arbitrary = liftM RSeqLeafNode $ sized (\n -> makeRSeqLeaf $ n+1)

intLimit = 0xffffff -- reasonably big but not ridiculous
data Pos = Pos Int deriving (Show, Eq, Ord)
instance Arbitrary Pos where
  arbitrary = liftM Pos $ choose (1, intLimit)
data NonNeg = NonNeg Int deriving (Show, Eq, Ord)
instance Arbitrary NonNeg where
  arbitrary = liftM NonNeg $ choose (0, intLimit)

mergeSubSeq_group =
  testGroup "mergeSubSeq" [
    testProperty  "out of leaf bounds1"       prop_leaf_bound1,
    testProperty  "out of leaf bounds2"       prop_leaf_bound2,
    testProperty  "seq end exceeds leaf span" prop_leaf_span
    ]
  where
    prop_leaf_bound1 :: 
      Pos -> RSeqLeafNode -> Pos -> Pos -> NonNeg -> Bool
    prop_leaf_bound1 (Pos off) (RSeqLeafNode rsleaf) (Pos s_off) (Pos s_sp) (NonNeg s_rpt) = 
      r_result == (CR.rSeqNodeLength rsleaf, rsleaf)
      where
        less_off
          | s_off < off     = s_off
          | (off-s_off) < 0 = 0
          | otherwise       = off-s_off
        r_result = CR.mergeSubSeq off rsleaf less_off s_sp s_rpt

    prop_leaf_bound2 :: 
      NonNeg -> RSeqLeafNode -> NonNeg -> Pos -> NonNeg -> Bool
    prop_leaf_bound2 (NonNeg off) (RSeqLeafNode rsleaf) (NonNeg s_off) (Pos s_sp) (NonNeg s_rpt) = 
      r_result == (rsleaf_len, rsleaf)
      where
        rsleaf_len = CR.rSeqNodeLength rsleaf
        more_off   = off + rsleaf_len + s_off
        r_result   = CR.mergeSubSeq off rsleaf more_off s_sp s_rpt

    prop_leaf_span :: 
      NonNeg -> RSeqLeafNode -> NonNeg -> Pos -> NonNeg -> Bool      
    prop_leaf_span (NonNeg off) (RSeqLeafNode rsleaf@(CR.RSeqLeaf rs_sp _)) (NonNeg s_off) (Pos s_sp) (NonNeg s_rpt) = 
      r_result == (rsleaf_len, rsleaf)
      where
        rsleaf_len = CR.rSeqNodeLength rsleaf
        s_off_mod  = s_off `mod` rs_sp
        s_sp'      = (rs_sp - s_off_mod) + s_sp
        r_result   = CR.mergeSubSeq off rsleaf (off+s_off_mod) s_sp' s_rpt
        