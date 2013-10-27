module Data.CycleRoll (
  RSequence(..),
  RSeqNode(..),
  roll,
  rSeqNodeLength
  ) where

import qualified Data.CycleRoll.SuffixArray as SA
import qualified Data.CycleRoll.Sequence as S

import qualified Data.Vector.Unboxed as UV
import qualified Data.Heap as Heap
import qualified Data.IntervalMap.Lazy as IvlMap
import qualified Data.IntervalMap.Interval as Ivl
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import Data.Maybe
import Debug.Trace

data RSequence = RSequence { offset :: Int, root :: RSeqNode } deriving (Show, Eq, Ord)
data RSeqNode =
  RSeqLeaf { span :: Int, repeat :: Int }
  | RSeqNode { repeat :: Int, subseqs :: [RSeqNode] }
  deriving (Show, Eq, Ord)

type IntervalMap = IvlMap.IntervalMap Int RSequence

{-
displayRSeqNode :: (UV.Unbox a) => Int -> UV.Vector a -> RSeqNode -> String
displayRSeqNode off input (RSeqLeaf sp rpt) = 
  (show $ UV.toList $ UV.slice off sp input) ++ "*" ++ (show rpt)
displayRSeqNode off input (RSeqNode rpt subs) =
  "(" ++ (show $ map (displayRSeqNode  subs)
-}

rSeqNodeLength :: RSeqNode -> Int
rSeqNodeLength (RSeqLeaf sp rpt) = sp*(rpt+1)
rSeqNodeLength (RSeqNode rpt subs) =
  (rpt+1)*(Foldable.foldl fn 0 subs)
  where
    fn l sub = l + (rSeqNodeLength sub)

rSequenceLength :: RSequence -> Int
rSequenceLength (RSequence _ rt) = rSeqNodeLength rt

foldRSeqNode :: 
  (Int -> a -> RSeqNode -> a) -> 
  Int -> 
  a -> 
  RSeqNode -> 
  (Int, a)
foldRSeqNode fn base_off base r@(RSeqLeaf _ _) =
  (rSeqNodeLength r, fn base_off base r)
foldRSeqNode fn base_off base (RSeqNode rpt subs) = 
  (r_len*(rpt+1), result)
  where
    f_fn (l, b) rsnode  = 
      let 
        (new_l, new_b) = foldRSeqNode fn l b rsnode
      in (l+new_l, new_b)
    
    (r_len, result) = foldl f_fn (base_off, base) subs
	
mergeSubSeq :: Int -> RSeqNode -> Int -> Int -> Int -> (Int, RSeqNode)
mergeSubSeq d_off d@(RSeqLeaf d_sp d_rpt) s_off s_sp s_rpt
  | d_off > s_off        = (d_len, d)
  | s_off >= d_off+d_len = (d_len, d)
  | s_end > d_sp         = (d_len, d)
  | s_len == d_sp        = (d_len, RSeqLeaf s_sp $ (d_rpt+1) * (s_rpt+1) - 1)
  | s_off2 == 0          = (d_len, RSeqNode d_rpt $ themid:thetail:[])
  | s_end == d_sp        = (d_len, RSeqNode d_rpt $ thehead:themid:[])
  | otherwise            = (d_len, RSeqNode d_rpt $ thehead:themid:thetail:[])
  where
    d_len      = rSeqNodeLength d
    s_off2     = s_off `mod` d_sp
    s_len      = S.length' s_sp s_rpt
    s_end      = s_off2 + s_len
    thehead    = RSeqLeaf s_off2 0
    themid     = RSeqLeaf s_sp s_rpt
    thetail    = RSeqLeaf (d_sp - s_end) 0

mergeSubSeq d_off (RSeqNode d_rpt subs) s_off s_sp s_rpt =
  (d_len, RSeqNode d_rpt $ reverse new_subseqs)
  where
    f_fn (l, ss) rsnode  = 
      let 
        (new_l, new_node) = mergeSubSeq (d_off+l) rsnode s_off s_sp s_rpt
      in (l+new_l, new_node:ss)
    
    (d_len, new_subseqs) = foldl f_fn (0, []) subs

mergeSequence :: RSequence -> S.Sequence -> RSequence
mergeSequence (RSequence off rt) (S.Sequence s_off s_sp s_rpt) =
  RSequence off $ snd $ mergeSubSeq off rt s_off s_sp s_rpt

addSeqToIMap :: IntervalMap -> S.Sequence -> IntervalMap
addSeqToIMap imap s@(S.Sequence off sp rpt)
  | s_independent     = IvlMap.insert ivl (RSequence off $ RSeqLeaf sp rpt) imap
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
        remain        = RSequence off $ RSeqLeaf (l - off) 0
        (_, min_rseq) = IvlMap.findMin imp
        min_off       = offset min_rseq
        min_len       = rSequenceLength min_rseq
        next_off      = min_off+min_len
        fill          = RSequence off $ RSeqLeaf (min_off - off) 0
        recurse       = process next_off (IvlMap.deleteMin imp)

-- trace ("roll got sequence: " ++ (show min_rseq) ++ "\n  recurse to " ++ (show next_off)) $ 
