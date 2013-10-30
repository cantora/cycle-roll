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

import qualified Debug.Trace as Trace
trace :: (Show a) => a -> a
trace x = Trace.trace (show x) x

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

makeRSeqLeaf :: Int -> Int -> CR.RSeqNode
makeRSeqLeaf n m =
  CR.RSeqLeaf q $ (n `div` q) - 1
  where
    find_divisor n m 
      | m > n           = error "m must be <= n"
      | n `mod` m == 0  = m
      | otherwise       = find_divisor n $ m+1
    q = find_divisor n m

makeRSeqLeafGen :: Int -> Gen CR.RSeqNode
makeRSeqLeafGen n = liftM (makeRSeqLeaf n) $ choose (1, n)

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
          else 
            return $ case subs of 
              ((CR.RSeqNode subs_rpt subs_subs):[])
                -> CR.RSeqNode ((subs_rpt+1)*(rpt+1) - 1) subs_subs
              ((CR.RSeqLeaf subs_sp subs_rpt):[])
                -> CR.RSeqLeaf subs_sp $ (subs_rpt+1)*(rpt+1) - 1
              _ -> CR.RSeqNode rpt subs                            

      make 0 = fail "cant make rseqnode with 0 size"
      make 1 = makeRSeqLeafGen 1
      make n = frequency [(1, makeRSeqLeafGen n), (1, make_node n)]


rSeqNode_group = 
  testGroup "rSeqNode" [
    testProperty "len"          prop_len,
    testProperty "visit_eq_len" prop_visit_eq_len,
    testProperty "visit_offset" prop_visit_offset,
    testCase     "visit_test1"  visit_test1
    ]
  where
    prop_len :: RSeqNodeWSz -> Bool
    prop_len (RSeqNodeWSz sz rsnode) = 
      sz == (CR.rSeqNodeLength rsnode)

    prop_visit_eq_len :: RSeqNodeWSz -> Bool
    prop_visit_eq_len (RSeqNodeWSz sz rsnode) = 
      let
        fn _ _ _ _ = ()
        result     = CR.visitRSeqNode fn 0 () rsnode
      in (CR.rSeqNodeLength rsnode) == fst result

    prop_visit_offset :: Int -> RSeqNodeWSz -> Bool
    prop_visit_offset start_off (RSeqNodeWSz sz rsnode) = 
      let
        fn _ _ _ _ = ()
        result     = CR.visitRSeqNode fn start_off () rsnode
      in (CR.rSeqNodeLength rsnode) == fst result

    visit_test1 =
      let 
        mp = 
          [
              0, 
                  4,
                  4+4,
                4,
                (8+4)+8,
              4,
              4+5*(8*2 + 6*3),
                174 + 3,
                  177 + 5*4,
                  197 + 3*2,
                177 + 5*4,
              174+3,
            0
            ]

        rseq = 
          CR.RSeqNode 0 [
            CR.RSeqLeaf 2 1,     -- 0
            CR.RSeqNode 4 [
              CR.RSeqNode 1 [
                CR.RSeqLeaf 4 0, -- 4
                CR.RSeqLeaf 1 3  -- 8
                ], -- 4
              CR.RSeqLeaf 6 2    -- (8+4)+8
              ], -- 4
            CR.RSeqLeaf 3 0,     -- 4+5*(8*2 + 6*3)
            CR.RSeqNode 1 [
              CR.RSeqLeaf 5 3,   -- 174+3
              CR.RSeqNode 3 [    
                CR.RSeqLeaf 3 1, -- 177+5*4
                CR.RSeqLeaf 2 3  -- 197+3*2
                ] -- 177 + 5*4
              ] -- 174+3
            ] -- 0

        fn off (idx, bl) _ (CR.RSeqLeaf _ _) = (idx+1, bl && (mp List.!! idx) == off)
        fn off (idx, bl) (ch_idx, ch_bl) _   = (ch_idx+1, bl && ch_bl && (mp List.!! idx) == off)
      in (13, True) @=? (snd $ CR.visitRSeqNode fn 0 (0, True) rseq)


data RSeqLeafNode = RSeqLeafNode CR.RSeqNode deriving (Show, Eq, Ord)
instance Arbitrary RSeqLeafNode where
  arbitrary = liftM RSeqLeafNode $ sized (\n -> makeRSeqLeafGen $ n+1)

saturateInt n
  | n > 0xffffff = 0xffffff -- reasonably big but not ridiculous
  | otherwise    = n

data Pos = Pos Int deriving (Show, Eq, Ord)
instance Arbitrary Pos where
  arbitrary = liftM Pos $ sized $ \n -> choose (1, saturateInt (n+1))

data NonNeg = NonNeg Int deriving (Show, Eq, Ord)
instance Arbitrary NonNeg where
  arbitrary = liftM NonNeg $ sized $ \n -> choose (0, saturateInt n)

mergeSubSeq_group =
  testGroup "mergeSubSeq" [
    testProperty  "length invariant"           prop_length_invariant,
    testProperty  "out of leaf bounds1"        prop_leaf_bound1,
    testProperty  "out of leaf bounds2"        prop_leaf_bound2,
    testProperty  "seq end exceeds leaf span"  prop_leaf_span,
    testProperty  "seq len equal to leaf span" prop_len_eq_span,
    testProperty  "seq off is 0"               prop_off_is_0,
    testProperty  "end is span"                prop_end_is_sp,
    testProperty  "seq in middle"              prop_seq_in_mid
    ]
  where
    prop_length_invariant :: NonNeg -> RSeqNodeWSz -> NonNeg -> NonNeg -> Bool
    prop_length_invariant (NonNeg off) (RSeqNodeWSz sz rsnode) (NonNeg s_off) (NonNeg s_len) =
      (CR.rSeqNodeLength res_node) == sz && res_off == (off+sz)
      where
        s_len'        = (s_len `mod` sz) + 1
        s_off_mod
          | (sz - s_len') < 1 = 0
          | otherwise         = s_off `mod` (sz - s_len')
        leaf_tpl (CR.RSeqLeaf sp rpt) = (sp, rpt)
        leaf_tpl _                    = error "expected a leaf"

        (s_sp, s_rpt)       = leaf_tpl $ makeRSeqLeaf sz s_len'
        (res_off, res_node) = trace $ Trace.traceShow (off, rsnode, (off+s_off_mod), s_sp, s_rpt) $ CR.mergeSubSeq off rsnode (off+s_off_mod) s_sp s_rpt

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

    prop_len_eq_span :: NonNeg -> NonNeg -> NonNeg -> Pos -> NonNeg -> Bool
    prop_len_eq_span (NonNeg off) (NonNeg rs_rpt) (NonNeg s_off) (Pos s_sp) (NonNeg s_rpt) = 
      r_result == (rsleaf_len, CR.RSeqLeaf s_sp $ (rs_rpt+1)*(s_rpt+1) - 1)
      where
        rs_sp      = s_sp * (s_rpt+1)
        rsleaf     = CR.RSeqLeaf rs_sp rs_rpt
        rsleaf_len = CR.rSeqNodeLength rsleaf
        s_off_mod' = (s_off `mod` rsleaf_len)
        s_off_mod  = s_off_mod' - (s_off_mod' `mod` rs_sp)
        r_result   = CR.mergeSubSeq off rsleaf (off+s_off_mod) s_sp s_rpt

    prop_off_is_0 ::
      NonNeg -> Pos -> NonNeg -> NonNeg -> Pos -> NonNeg -> Bool      
    prop_off_is_0 (NonNeg off) (Pos rs_sp) (NonNeg rs_rpt) (NonNeg s_off) (Pos s_sp) (NonNeg s_rpt)
      | res_len /= rsleaf_len             = False
      | subs_amt res_node /= 2            = False
      | CR.repeat res_node /= rs_rpt      = False
      | mid res_node /= (s_sp, s_rpt)     = False
      | end res_node /= (rs_sp'-s_end, 0) = False
      | otherwise                         = True
      where
        rs_sp' = (s_sp * (s_rpt+1)) + rs_sp
        s_off_mod 
          | rs_rpt < 1 = 0
          | otherwise  = (s_off `mod` rs_rpt) * rs_sp'

        rsleaf              = CR.RSeqLeaf rs_sp' rs_rpt
        rsleaf_len          = CR.rSeqNodeLength rsleaf
        s_off_mod'          = s_off_mod `mod` rs_sp'
        s_end               = s_off_mod' + (s_sp*(s_rpt+1))
        (res_len, res_node) = CR.mergeSubSeq off rsleaf (off+s_off_mod) s_sp s_rpt
        subs_amt            = CR.rSeqNodeSize

        rsleaf_tpl (CR.RSeqLeaf sp rpt) = (sp, rpt)
        mid (CR.RSeqNode _ (x:xs))      = rsleaf_tpl x
        end (CR.RSeqNode _ (_:x:xs))    = rsleaf_tpl x

    prop_end_is_sp ::
      NonNeg -> Pos -> NonNeg -> NonNeg -> Pos -> NonNeg -> Bool      
    prop_end_is_sp (NonNeg off) (Pos rs_sp) (NonNeg rs_rpt) (NonNeg s_off) (Pos s_sp) (NonNeg s_rpt)
      | res_len /= rsleaf_len          = False
      | subs_amt res_node /= 2         = False
      | CR.repeat res_node /= rs_rpt   = False
      | hd res_node /= (rs_sp, 0)      = False
      | mid res_node /= (s_sp, s_rpt)  = False
      | otherwise                      = True
      where
        rs_sp' = rs_sp + (s_sp * (s_rpt+1))
        s_off_mod
          | rs_rpt < 1 = rs_sp
          | otherwise  = (s_off `mod` rs_rpt) * rs_sp' + rs_sp

        rsleaf              = CR.RSeqLeaf rs_sp' rs_rpt
        rsleaf_len          = CR.rSeqNodeLength rsleaf
        (res_len, res_node) = CR.mergeSubSeq off rsleaf (off+s_off_mod) s_sp s_rpt
        subs_amt            = CR.rSeqNodeSize

        rsleaf_tpl (CR.RSeqLeaf sp rpt) = (sp, rpt)
        hd (CR.RSeqNode _ (x:xs))       = rsleaf_tpl x
        mid (CR.RSeqNode _ (_:x:xs))    = rsleaf_tpl x

    prop_seq_in_mid ::
      NonNeg -> Pos -> NonNeg -> NonNeg -> Pos -> NonNeg -> Bool      
    prop_seq_in_mid (NonNeg off) (Pos rs_sp) (NonNeg rs_rpt) (NonNeg s_off) (Pos s_sp) (NonNeg s_rpt)
      | res_len /= rsleaf_len             = False
      | subs_amt res_node /= 3            = False
      | CR.repeat res_node /= rs_rpt      = False
      | hd res_node /= (s_off_mod', 0)    = False
      | mid res_node /= (s_sp, s_rpt)     = False
      | end res_node /= (rs_sp'-s_end, 0) = False
      | otherwise                         = True
      where
        rs_sp' = rs_sp + 1 + (s_sp * (s_rpt+1))
        s_off_mod
          | rs_rpt < 1 = off_in_span
          | otherwise  = (s_off `mod` rs_rpt) * rs_sp' + off_in_span
          where
            off_in_span = (s_off `mod` rs_sp) + 1

        rsleaf              = CR.RSeqLeaf rs_sp' rs_rpt
        rsleaf_len          = CR.rSeqNodeLength rsleaf
        (res_len, res_node) = CR.mergeSubSeq off rsleaf (off+s_off_mod) s_sp s_rpt
        subs_amt            = CR.rSeqNodeSize
        s_off_mod'          = s_off_mod `mod` rs_sp'
        s_end               = s_off_mod' + (s_sp*(s_rpt+1))

        rsleaf_tpl (CR.RSeqLeaf sp rpt) = (sp, rpt)
        hd (CR.RSeqNode _ (x:xs))       = rsleaf_tpl x
        mid (CR.RSeqNode _ (_:x:xs))    = rsleaf_tpl x
        end (CR.RSeqNode _ (_:_:x:xs))  = rsleaf_tpl x

