module Data.CycleRoll.SubSeqTest (
  group
  ) where

import Test.Utils -- our common testing stuff
import qualified Data.CycleRoll.RSeqNode as RSeq
import qualified Data.CycleRoll.SubSeq as SubSeq

import qualified Data.Sequence as DSeq
import qualified Data.List as List

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Control.Monad

--import qualified Debug.Trace as Trace
--trace :: (Show a) => a -> a
--trace x = Trace.trace (show x) x

group = 
  testGroup "SubSeq" [
    merge_group
    ]

data RSeqLeafNode = RSeqLeafNode RSeq.Node deriving (Show, Eq, Ord)
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

merge_group =
  testGroup "merge" [
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
      (RSeq.length res_node) == sz && res_off == sz
      where
        s_len'        = (s_len `mod` sz) + 1
        s_off_mod
          | (sz - s_len') < 1 = 0
          | otherwise         = s_off `mod` (sz - s_len')
        leaf_tpl (RSeq.Leaf sp rpt) = (sp, rpt)
        leaf_tpl _                    = error "expected a leaf"

        (s_sp, s_rpt)       = leaf_tpl $ makeRSeqLeaf sz s_len'
        -- (res_off, res_node) = trace $ Trace.traceShow (off, rsnode, (off+s_off_mod), s_sp, s_rpt)
        (res_off, res_node) = SubSeq.merge off rsnode (off+s_off_mod) s_sp s_rpt

    prop_leaf_bound1 ::
      Pos -> RSeqLeafNode -> Pos -> Pos -> NonNeg -> Bool
    prop_leaf_bound1 (Pos off) (RSeqLeafNode rsleaf) (Pos s_off) (Pos s_sp) (NonNeg s_rpt) = 
      r_result == (RSeq.length rsleaf, rsleaf)
      where
        less_off
          | s_off < off     = s_off
          | (off-s_off) < 0 = 0
          | otherwise       = off-s_off
        r_result = SubSeq.merge off rsleaf less_off s_sp s_rpt

    prop_leaf_bound2 :: 
      NonNeg -> RSeqLeafNode -> NonNeg -> Pos -> NonNeg -> Bool
    prop_leaf_bound2 (NonNeg off) (RSeqLeafNode rsleaf) (NonNeg s_off) (Pos s_sp) (NonNeg s_rpt) = 
      r_result == (rsleaf_len, rsleaf)
      where
        rsleaf_len = RSeq.length rsleaf
        more_off   = off + rsleaf_len + s_off
        r_result   = SubSeq.merge off rsleaf more_off s_sp s_rpt

    prop_leaf_span :: 
      NonNeg -> RSeqLeafNode -> NonNeg -> Pos -> NonNeg -> Bool      
    prop_leaf_span (NonNeg off) (RSeqLeafNode rsleaf@(RSeq.Leaf rs_sp _)) (NonNeg s_off) (Pos s_sp) (NonNeg s_rpt) = 
      r_result == (rsleaf_len, rsleaf)
      where
        rsleaf_len = RSeq.length rsleaf
        s_off_mod  = s_off `mod` rs_sp
        s_sp'      = (rs_sp - s_off_mod) + s_sp
        r_result   = SubSeq.merge off rsleaf (off+s_off_mod) s_sp' s_rpt

    prop_len_eq_span :: NonNeg -> NonNeg -> NonNeg -> Pos -> NonNeg -> Bool
    prop_len_eq_span (NonNeg off) (NonNeg rs_rpt) (NonNeg s_off) (Pos s_sp) (NonNeg s_rpt) = 
      r_result == (rsleaf_len, RSeq.Leaf s_sp $ (rs_rpt+1)*(s_rpt+1) - 1)
      where
        rs_sp      = s_sp * (s_rpt+1)
        rsleaf     = RSeq.Leaf rs_sp rs_rpt
        rsleaf_len = RSeq.length rsleaf
        s_off_mod' = (s_off `mod` rsleaf_len)
        s_off_mod  = s_off_mod' - (s_off_mod' `mod` rs_sp)
        r_result   = SubSeq.merge off rsleaf (off+s_off_mod) s_sp s_rpt

    prop_off_is_0 ::
      NonNeg -> Pos -> NonNeg -> NonNeg -> Pos -> NonNeg -> Bool      
    prop_off_is_0 (NonNeg off) (Pos rs_sp) (NonNeg rs_rpt) (NonNeg s_off) (Pos s_sp) (NonNeg s_rpt)
      | res_len /= rsleaf_len             = False
      | subs_amt res_node /= 2            = False
      | RSeq.repeat res_node /= rs_rpt    = False
      | mid res_node /= (s_sp, s_rpt)     = False
      | end res_node /= (rs_sp'-s_end, 0) = False
      | otherwise                         = True
      where
        rs_sp' = (s_sp * (s_rpt+1)) + rs_sp
        s_off_mod 
          | rs_rpt < 1 = 0
          | otherwise  = (s_off `mod` rs_rpt) * rs_sp'

        rsleaf              = RSeq.Leaf rs_sp' rs_rpt
        rsleaf_len          = RSeq.length rsleaf
        s_off_mod'          = s_off_mod `mod` rs_sp'
        s_end               = s_off_mod' + (s_sp*(s_rpt+1))
        (res_len, res_node) = SubSeq.merge off rsleaf (off+s_off_mod) s_sp s_rpt
        subs_amt            = RSeq.size

        rsleaf_tpl (RSeq.Leaf sp rpt) = (sp, rpt)
        rsleaf_tpl _                  = error "expected a leaf"

        mid (RSeq.Node _ subs)        = rsleaf_tpl $ DSeq.index subs 0
        end (RSeq.Node _ subs)        = rsleaf_tpl $ DSeq.index subs 1

    prop_end_is_sp ::
      NonNeg -> Pos -> NonNeg -> NonNeg -> Pos -> NonNeg -> Bool      
    prop_end_is_sp (NonNeg off) (Pos rs_sp) (NonNeg rs_rpt) (NonNeg s_off) (Pos s_sp) (NonNeg s_rpt)
      | res_len /= rsleaf_len          = False
      | subs_amt res_node /= 2         = False
      | RSeq.repeat res_node /= rs_rpt = False
      | hd res_node /= (rs_sp, 0)      = False
      | mid res_node /= (s_sp, s_rpt)  = False
      | otherwise                      = True
      where
        rs_sp' = rs_sp + (s_sp * (s_rpt+1))
        s_off_mod
          | rs_rpt < 1 = rs_sp
          | otherwise  = (s_off `mod` rs_rpt) * rs_sp' + rs_sp

        rsleaf              = RSeq.Leaf rs_sp' rs_rpt
        rsleaf_len          = RSeq.length rsleaf
        (res_len, res_node) = SubSeq.merge off rsleaf (off+s_off_mod) s_sp s_rpt
        subs_amt            = RSeq.size

        rsleaf_tpl (RSeq.Leaf sp rpt) = (sp, rpt)
        rsleaf_tpl _                  = error "expected a leaf"

        hd  (RSeq.Node _ subs)        = rsleaf_tpl $ DSeq.index subs 0
        mid (RSeq.Node _ subs)        = rsleaf_tpl $ DSeq.index subs 1

    prop_seq_in_mid ::
      NonNeg -> Pos -> NonNeg -> NonNeg -> Pos -> NonNeg -> Bool      
    prop_seq_in_mid (NonNeg off) (Pos rs_sp) (NonNeg rs_rpt) (NonNeg s_off) (Pos s_sp) (NonNeg s_rpt)
      | res_len /= rsleaf_len             = False
      | subs_amt res_node /= 3            = False
      | RSeq.repeat res_node /= rs_rpt    = False
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

        rsleaf              = RSeq.Leaf rs_sp' rs_rpt
        rsleaf_len          = RSeq.length rsleaf
        (res_len, res_node) = SubSeq.merge off rsleaf (off+s_off_mod) s_sp s_rpt
        subs_amt            = RSeq.size
        s_off_mod'          = s_off_mod `mod` rs_sp'
        s_end               = s_off_mod' + (s_sp*(s_rpt+1))

        rsleaf_tpl (RSeq.Leaf sp rpt) = (sp, rpt)
        rsleaf_tpl _                  = error "expected a leaf"

        hd  (RSeq.Node _ subs)        = rsleaf_tpl $ DSeq.index subs 0
        mid (RSeq.Node _ subs)        = rsleaf_tpl $ DSeq.index subs 1
        end (RSeq.Node _ subs)        = rsleaf_tpl $ DSeq.index subs 2
