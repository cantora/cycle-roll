module Data.CycleRoll (
  RSequence(..),
  RSeqNode(..),
  roll,
  rSeqNodeLength,
  rSeqNodeSize,
  txfmRSeqNode,
  TxfmDir(..),
  foldRSeqLeaves,
  mergeSubSeq,
  displayRSeqNode,
  displayRSequence,
  displaySequences
  ) where

import qualified Data.CycleRoll.Sequence as S

import qualified Data.Heap as Heap
import qualified Data.IntervalMap.Lazy as IvlMap
import qualified Data.IntervalMap.Interval as Ivl
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Vector.Unboxed as UV
import Data.Maybe
--import Debug.Trace

data RSequence = RSequence { offset :: Int, root :: RSeqNode } deriving (Show, Eq, Ord)
data RSeqNode =
  RSeqLeaf { span :: Int, repeat :: Int }
  | RSeqNode { repeat :: Int, subseqs :: [RSeqNode] }
  deriving (Show, Eq, Ord)

type IntervalMap = IvlMap.IntervalMap Int RSequence

displayRSeqNode :: (Show a, UV.Unbox a) => Int -> UV.Vector a -> RSeqNode -> String
displayRSeqNode base_off input rsnode = 
  snd $ txfmRSeqNode fn "" rsnode
  where
    slice off sp = show $ UV.toList $ UV.slice (base_off+off) sp input
    fn off str (RSeqLeaf sp rpt)
      | rpt > 0   = base $ "*" ++ (show (rpt+1))
      | otherwise = base ""
      where
        base suf = TxfmConst $ str ++ (slice off sp) ++ suf

    fn _ str (RSeqNode rpt _)
      | rpt > 0   = base $ "*" ++ (show (rpt+1))
      | otherwise = base ""
      where
        base suf = TxfmCB $ \ch_str -> str ++ "(" ++ ch_str ++ ")" ++ suf

displayRSequence :: (Show a, UV.Unbox a) => UV.Vector a -> RSequence -> String
displayRSequence input (RSequence off r) = displayRSeqNode off input r

displaySequences :: (Show a, UV.Unbox a) => UV.Vector a -> [RSequence] -> String
displaySequences input xs = List.intercalate ", " $ List.map (displayRSequence input) xs

--total length of all the leaf nodes combined
rSeqNodeLength :: RSeqNode -> Int
rSeqNodeLength = 
  fst . (foldRSeqLeaves (\_ _ _ -> ()) ())

--number of leaf nodes in the recursive sequence
rSeqNodeSize :: RSeqNode -> Int
rSeqNodeSize = 
  snd . (foldRSeqLeaves (\_ n _ -> n+1) 0)

rSequenceLength :: RSequence -> Int
rSequenceLength (RSequence _ rt) = rSeqNodeLength rt

--rSequenceSize :: RSequence -> Int
--rSequenceSize (RSequence _ rt) = rSeqNodeSize rt

data TxfmDir a = TxfmConst a       -- use 'a' value as new accumulator, dont process children
                 | TxfmCB (a -> a) -- pass processed children to function, 
                                   -- process children using default accumulator
                 | TxfmPair a (a -> a)

txfmRSeqNode ::
  ( Int ->         -- current offset
    a ->           -- accumulator
    RSeqNode ->    -- current node
    TxfmDir a) ->  -- directive which specifies how to process children
  a ->           -- accumulator
  RSeqNode ->    -- root node
  (Int, a)       -- final offset of node, accumulator result
txfmRSeqNode fn base_acc rsnode =
  visit 0 0 base_acc rsnode
  where
    recurse off (sz, acc) n = visit off sz acc n

    visit off sz acc r@(RSeqLeaf sp rpt) =
      (sz + (S.length' sp rpt), result tdir)
      where
        tdir = fn (off+sz) acc r
        result (TxfmConst new_acc) = new_acc
        result (TxfmCB cb)         = cb base_acc
        result (TxfmPair base cb)  = cb base

    visit off sz acc r@(RSeqNode rpt subs) =
      (sz + (S.length' rlen rpt), acc_result)
      where
        tdir = fn (off+sz) acc r
        (rlen, acc_result) = result tdir
        ch_result cb base =
          (\(rl, a) -> (rl, cb a)) $ foldl (recurse (off+sz)) (0, base) subs
        result (TxfmConst new_acc)    = (fst $ ch_result id base_acc, new_acc)
        result (TxfmCB cb)            = ch_result cb base_acc
        result (TxfmPair new_base cb) = ch_result cb new_base

foldRSeqLeaves :: 
  ( Int ->       -- current offset
    a ->         -- accumulator
    RSeqNode ->  -- current node
    a) ->        -- returns new accumulator
  a ->           -- accumulator
  RSeqNode ->    -- root node
  (Int, a)       -- length of node, accumulator result
foldRSeqLeaves fn base rsnode =
  txfmRSeqNode vfn base rsnode
  where
    vfn curr_off acc r@(RSeqLeaf _ _) = TxfmConst $ fn curr_off acc r
    vfn _        acc _                = TxfmPair acc id

mergeSubSeq :: 
  Int ->          -- offset of node
  RSeqNode ->     -- node
  Int ->          -- offset of sequence
  Int ->          -- span of sequence
  Int ->          -- repetitions of sequence
  (Int, RSeqNode) -- returns size of new node, new node
mergeSubSeq rs_off rsnode s_off s_sp s_rpt =
  (node_len, head nodes)
  where
    (node_len, nodes) = txfmRSeqNode fn [] rsnode
    fn _ acc (RSeqNode rpt _) = TxfmCB $ \ch ->
      (RSeqNode rpt $ reverse ch):acc
    fn off acc d@(RSeqLeaf d_sp d_rpt) = TxfmConst $ leaf_fn off acc d d_sp d_rpt
    leaf_fn off acc d d_sp d_rpt
      | d_off > s_off
        || s_off >= d_off+d_len 
        || s_end > d_sp          = d:acc
      | s_len == d_sp            = (RSeqLeaf s_sp $ (d_rpt+1) * (s_rpt+1) - 1):acc
      | s_off2 == 0              = new_node $ themid:thetail:[]
      | s_end == d_sp            = new_node $ thehead:themid:[]
      | otherwise                = new_node $ thehead:themid:thetail:[]
      where
        d_off      = rs_off + off
        d_len      = rSeqNodeLength d
        s_off2     = (s_off-d_off) `mod` d_sp
        s_len      = S.length' s_sp s_rpt
        s_end      = s_off2 + s_len
        thehead    = RSeqLeaf s_off2 0
        themid     = RSeqLeaf s_sp s_rpt
        thetail    = RSeqLeaf (d_sp - s_end) 0

        new_node subs = (RSeqNode d_rpt subs):acc

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
