module Data.CycleRoll (
  RSequence(..),
  roll,
  mergeSubSeq,
  displayRSequence,
  displaySequences
  ) where

import qualified Data.CycleRoll.Sequence as S
import qualified Data.CycleRoll.RSeqNode as RSeq

import qualified Data.Heap as Heap
import qualified Data.IntervalMap.Lazy as IvlMap
import qualified Data.IntervalMap.Interval as Ivl
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Vector.Unboxed as UV
import Data.Maybe
--import Debug.Trace

data RSequence = RSequence { offset :: Int, root :: RSeq.Node } deriving (Show, Eq, Ord)

type IntervalMap = IvlMap.IntervalMap Int RSequence

displayRSequence :: (Show a, UV.Unbox a) => UV.Vector a -> RSequence -> String
displayRSequence input (RSequence off r) = RSeq.display off input r

displaySequences :: (Show a, UV.Unbox a) => UV.Vector a -> [RSequence] -> String
displaySequences input xs = List.intercalate ", " $ List.map (displayRSequence input) xs

rSequenceLength :: RSequence -> Int
rSequenceLength (RSequence _ rt) = RSeq.length rt

mergeSubSeq :: 
  Int ->           -- offset of node
  RSeq.Node ->     -- node
  Int ->           -- offset of sequence
  Int ->           -- span of sequence
  Int ->           -- repetitions of sequence
  (Int, RSeq.Node) -- returns size of new node, new node
mergeSubSeq rs_off rsnode s_off s_sp s_rpt =
  (node_len, result_node nodes)
  where
    result_node []          = error "this shouldnt happen"
    result_node (node:[])   = node
    result_node l           = RSeq.Node 0 (reverse l)
    (node_len, nodes) = RSeq.transform fn [] rsnode

    fn _ acc (RSeq.Node rpt _) = 
      RSeq.TxfmCB $ \ch -> (RSeq.Node rpt $ reverse ch):acc
    fn off acc d@(RSeq.Leaf d_sp d_rpt) =
      RSeq.TxfmConst $ leaf_fn off acc d d_sp d_rpt
    leaf_fn off acc d d_sp d_rpt
      | d_off > s_off
        || s_off >= d_off+d_len 
        || s_end > d_sp          = d:acc
      | s_len == d_sp            = (RSeq.Leaf s_sp $ (d_rpt+1) * (s_rpt+1) - 1):acc
      | s_off2 == 0              = new_node $ themid:thetail:[]
      | s_end == d_sp            = new_node $ thehead:themid:[]
      | otherwise                = new_node $ thehead:themid:thetail:[]
      where
        d_off      = rs_off + off
        d_len      = RSeq.length d
        s_off2     = (s_off-d_off) `mod` d_sp
        s_len      = S.length' s_sp s_rpt
        s_end      = s_off2 + s_len
        thehead    = RSeq.Leaf s_off2 0
        themid     = RSeq.Leaf s_sp s_rpt
        thetail    = RSeq.Leaf (d_sp - s_end) 0

        new_node subs
          | d_rpt > 0    = (RSeq.Node d_rpt subs):acc
          | otherwise    = (reverse subs) ++ acc

mergeSequence :: RSequence -> S.Sequence -> RSequence
mergeSequence (RSequence off rt) (S.Sequence s_off s_sp s_rpt) =
  RSequence off $ snd $ mergeSubSeq off rt s_off s_sp s_rpt

addSeqToIMap :: IntervalMap -> S.Sequence -> IntervalMap
addSeqToIMap imap s@(S.Sequence off sp rpt)
  | s_independent     = IvlMap.insert ivl (RSequence off $ RSeq.Leaf sp rpt) imap
  | isJust dominator  = IvlMap.insert dom_ivl new_dom_seq imap
  | otherwise         = imap
  where
    ivl   = IvlMap.IntervalCO off $ S.end s
    xsecs = IvlMap.intersecting imap ivl

    s_independent      = null xsecs
    dominator          = List.find (flip (Ivl.subsumes . fst) ivl) xsecs
    (dom_ivl, dom_seq) = fromJust dominator
    new_dom_seq        = mergeSequence dom_seq s

sequenceIntervals :: [Heap.Heap S.Sequence] -> IntervalMap
sequenceIntervals seqs =
  foldl (Foldable.foldl process_seq) IvlMap.empty seqs
  where
    process_seq imap (S.Sequence off sp rpt) =
      let 
        seq_i i = S.Sequence off sp i
        add_seq_to_imap imp i = addSeqToIMap imp $ seq_i i
      in foldl add_seq_to_imap imap (reverse [1..rpt])
-- trace ("add sequence " ++ (show $ seq_i i) ++ "\ncurrent imap: " ++ (show imp)) $ 

roll :: Int -> [Heap.Heap S.Sequence] -> [RSequence]
roll src_len sequences =
  process 0 imap
  where
    imap = sequenceIntervals sequences
    l    = src_len
    process off imp
      | IvlMap.null imp  = remain:[]
      | off >= l         = []
      | off < min_off    = fill:min_rseq:recurse
      | off == min_off   = min_rseq:recurse
      | otherwise        = error "this shouldnt happen"
      where
        remain        = RSequence off $ RSeq.Leaf (l - off) 0
        (_, min_rseq) = IvlMap.findMin imp
        min_off       = offset min_rseq
        min_len       = rSequenceLength min_rseq
        next_off      = min_off+min_len
        fill          = RSequence off $ RSeq.Leaf (min_off - off) 0
        recurse       = process next_off (IvlMap.deleteMin imp)

-- trace ("roll got sequence: " ++ (show min_rseq) ++ "\n  recurse to " ++ (show next_off)) $ 
