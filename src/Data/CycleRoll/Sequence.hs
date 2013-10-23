module Data.CycleRoll.Sequence where

import qualified Data.CycleRoll.LCP as LCP

import qualified Data.Heap as Heap
import qualified Data.Foldable as Foldable
import Debug.Trace

data Sequence = 
  Sequence {
    offset :: Int,
    span :: Int,
    repeats :: Int
    } deriving (Show, Eq, Ord)

lcpGroupSequences :: (Int -> Int -> Int -> Bool) -> LCP.Group -> [Sequence]
lcpGroupSequences lcp_rmq (LCP.Group lcp memb) =
  finish $ Foldable.foldl proc_el (Heap.empty, []) memb
  where
    cons_partial pseq rpt ss 
      | rpt > 0   = pseq:ss
      | otherwise = ss

    finish (remain, seqs) = 
      reverse $ Foldable.foldr proc_partial seqs remain
      where
        proc_partial (_, _, sq@(Sequence _ _ rpt)) = cons_partial sq rpt
       
    proc_el (partials, seqs) (LCP.GroupElem src_idx sa_idx) =
      recurse seqs partials
      where
        recurse new_seqs ps
          | Heap.null ps        = base
          | target > src_idx    = base
          | target < src_idx    = recurse next_ss (Heap.deleteMin ps)
          | prefixes_match      = (Heap.insert ext_partial (Heap.deleteMin ps), new_seqs)
          | otherwise           = (base_ps, next_ss)
          where
            base_ps  = Heap.insert (src_idx+lcp, sa_idx, Sequence src_idx lcp 0) ps
            base     = (base_ps, new_seqs)

            (target, sa_idx2, seqnce@(Sequence off sp rpt)) = Heap.minimum ps

            next_ss        = cons_partial seqnce rpt new_seqs
            ext_partial    = (src_idx+lcp, sa_idx, Sequence off sp (rpt+1))
            prefixes_match = (lcp_rmq sa_idx sa_idx2 lcp)

            debug tpl   = trace (show tpl) tpl

sequences :: [LCP.Group] -> (Int -> Int -> Int -> Bool) -> [Sequence]
sequences merged_grps lcp_rmq = concat $ map (lcpGroupSequences lcp_rmq) merged_grps
