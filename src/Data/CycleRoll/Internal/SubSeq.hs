module Data.CycleRoll.Internal.SubSeq (
  merge
  ) where

import qualified Data.CycleRoll.Internal.Sequence as S
import qualified Data.CycleRoll.Internal.RSequence.Node as RSNode
import Data.CycleRoll.Internal.RSequence.Node (Node(..))

import qualified Data.Sequence as DSeq
import Data.Sequence ((|>), (><))

merge ::
  Int ->           -- offset of node
  Node ->          -- node
  Int ->           -- offset of sequence
  Int ->           -- span of sequence
  Int ->           -- repetitions of sequence
  (Int, Node)      -- returns size of new node, new node
merge rs_off rsnode s_off s_sp s_rpt =
  (node_len, result_node)
  where
    (node_len, nodes) = RSNode.transform fn DSeq.empty rsnode
    result_node = case (DSeq.length nodes) of
      0 -> error "this shouldnt happen"
      1 -> DSeq.index nodes 0
      _ -> Node 0 nodes

    fn _ acc (Node rpt _) = 
      RSNode.TxfmCB $ \ch -> acc |> (Node rpt ch)
    fn off acc d@(Leaf d_sp d_rpt) =
      RSNode.TxfmConst $ leaf_fn off acc d d_sp d_rpt
    leaf_fn off acc d d_sp d_rpt
      | d_off > s_off
        || s_off >= d_end
        || s_end > d_sp    = acc |> d
      | s_len == d_sp      = acc |> new_leaf
      | s_off2 == 0        = new_node $ themid:thetail:[]
      | s_end == d_sp      = new_node $ thehead:themid:[]
      | otherwise          = new_node $ thehead:themid:thetail:[]
      where
        d_off      = rs_off + off
        d_len      = RSNode.length d
        d_end      = d_off+d_len
        s_off2     = (s_off-d_off) `mod` d_sp
        s_len      = S.length' s_sp s_rpt
        s_end      = s_off2 + s_len
        thehead    = Leaf s_off2 0
        themid     = Leaf s_sp s_rpt
        thetail    = Leaf (d_sp - s_end) 0
        new_leaf   = Leaf s_sp $ (d_rpt+1) * (s_rpt+1) - 1

        new_node subs
          | d_rpt > 0    = acc |> (Node d_rpt subs')
          | otherwise    = acc >< subs'
          where
            subs' = DSeq.fromList subs

