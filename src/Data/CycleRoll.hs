module Data.CycleRoll (
  RSequence(..),
  RSeqNode(..),
  roll,
  rSeqNodeLength,
  rSeqNodeSize,
  visitRSeqNode,
  foldRSeqLeaves,
  mergeSubSeq
  ) where

import qualified Data.CycleRoll.Sequence as S

import qualified Data.Heap as Heap
import qualified Data.IntervalMap.Lazy as IvlMap
import qualified Data.IntervalMap.Interval as Ivl
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import Data.Maybe
--import Debug.Trace

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

--total length of all the leaf nodes combined
rSeqNodeLength :: RSeqNode -> Int
rSeqNodeLength = 
  fst . (foldRSeqLeaves (\_ _ _ -> ()) 0 ())

--number of leaf nodes in the recursive sequence
rSeqNodeSize :: RSeqNode -> Int
rSeqNodeSize = 
  snd . (foldRSeqLeaves (\_ n _ -> n+1) 0 0)

rSequenceLength :: RSequence -> Int
rSequenceLength (RSequence _ rt) = rSeqNodeLength rt

--rSequenceSize :: RSequence -> Int
--rSequenceSize (RSequence _ rt) = rSeqNodeSize rt

visitRSeqNode :: 
  ( Int ->       -- current offset
    a ->         -- sibling accumulator
    a ->         -- children accumulator
    RSeqNode ->  -- current node. 
    a) ->        -- returns new accumulator
  Int ->         -- start offset
  a ->           -- base accumulator
  RSeqNode ->    -- root node
  (Int, a)       -- size of node, accumulator result
visitRSeqNode fn start_off base_accum rsnode =
  visit 0 base_accum base_accum rsnode
  where
    visit curr_len sib_accum r@(RSeqLeaf sp rpt) =
      (sp*(rpt+1), fn (start_off+curr_len) sib_accum base_accum r)
    visit curr_len sib_accum r@(RSeqNode rpt subs) = 
      (subs_len*(rpt+1), fn (start_off+curr_len) sib_base result r)
      where
        f_fn (l, b) rsnode = 
          let 
            (nxt_l, nxt_b) = visit (curr_len+l) b rsnode 
          in (l+nxt_l, nxt_b)

        (subs_len, result) = foldl f_fn (0, base_accum) subs

visitRSeqNode :: 
  ( Int ->          -- current offset
    a ->            -- accumulator
    RSeqNode ->     -- current node
    ( a ->            -- accumulator for children
      [RSeqNode] ->   -- children to visit
      (Int, a, RSeqNode))  ->   -- total length of children, 
    (Int, a, RSeqNode)) ->    -- returns size of this node's children and new accumulator
  Int ->          -- start offset
  a ->            -- base accumulator
  RSeqNode ->     -- root node
  (Int, a, RSeqNode)        -- size of node, accumulator result
visitRSeqNode fn start_off base_accum rsnode =
  visit 0 base_accum rsnode
  where
    visit curr_len acc node =
      (n_len, result)
      where
        (ch_len, result) = fn (start_off+curr_len) node acc vis
        (vis, get_len)   = y node
        y (RSeqLeaf sp rpt) = 
          (leaf_vis, ch_len*(sp*



    visit curr_len sib_accum r@(RSeqLeaf sp rpt) =
      (sp*(rpt+1), fn (start_off+curr_len) sib_accum base_accum r)
    visit curr_len sib_accum r@(RSeqNode rpt subs) = 
      (subs_len*(rpt+1), fn (start_off+curr_len) sib_base result r)
      where
        f_fn (l, b) rsnode = 
          let 
            (nxt_l, nxt_b) = visit (curr_len+l) b rsnode 
          in (l+nxt_l, nxt_b)

        (subs_len, result) = foldl f_fn (0, base_accum) subs


foldRSeqLeaves :: 
  ( Int ->       -- current offset
    a ->         -- accumulator
    RSeqNode ->  -- current node
    a) ->        -- returns new accumulator
  Int ->         -- start offset
  a ->           -- accumulator
  RSeqNode ->    -- root node
  (Int, a)       -- final offset of node, accumulator result
foldRSeqLeaves fn start_off base rsnode =
  visitRSeqNode vfn start_off base rsnode
  where
    vfn curr_off acc _      r@(RSeqLeaf _ _) = fn curr_off acc r
    vfn _        _   ch_acc _                = ch_acc

mergeSubSeq :: Int -> RSeqNode -> Int -> Int -> Int -> (Int, RSeqNode)
mergeSubSeq rs_off rsnode s_off s_sp s_rpt =
  (node_len, head nodes)
  where
    (node_len, nodes) = visitRSeqNode fn rs_off [] rsnode
    fn _ s_ns ch_ns (RSeqNode d_rpt _) = 
      (RSeqNode d_rpt $ reverse ch_ns):s_ns
    fn d_off sibs _ d@(RSeqLeaf d_sp d_rpt)
      | d_off > s_off        = ret d
      | s_off >= d_off+d_len = ret d
      | s_end > d_sp         = ret d
      | s_len == d_sp        = ret $ RSeqLeaf s_sp $ (d_rpt+1) * (s_rpt+1) - 1
      | s_off2 == 0          = ret $ RSeqNode d_rpt $ themid:thetail:[]
      | s_end == d_sp        = ret $ RSeqNode d_rpt $ thehead:themid:[]
      | otherwise            = ret $ RSeqNode d_rpt $ thehead:themid:thetail:[]
      where
        d_len      = rSeqNodeLength d
        s_off2     = (s_off-d_off) `mod` d_sp
        s_len      = S.length' s_sp s_rpt
        s_end      = s_off2 + s_len
        thehead    = RSeqLeaf s_off2 0
        themid     = RSeqLeaf s_sp s_rpt
        thetail    = RSeqLeaf (d_sp - s_end) 0
        ret x      = x:sibs

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
