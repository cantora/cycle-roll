module Data.CycleRoll (
  RSequence(..),
  roll,
  displayRSequence,
  displaySequences
  ) where

import qualified Data.CycleRoll.Sequence as S
import qualified Data.CycleRoll.RSeqNode as RSeq
import qualified Data.CycleRoll.SubSeq as SubSeq

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

mergeSequence :: RSequence -> S.Sequence -> RSequence
mergeSequence (RSequence off rt) (S.Sequence s_off s_sp s_rpt) =
  RSequence off $ snd $ SubSeq.merge off rt s_off s_sp s_rpt

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
