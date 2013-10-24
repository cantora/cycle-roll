module Data.CycleRoll.Sequence where

import qualified Data.CycleRoll.LCP as LCP

import qualified Data.Heap as Heap
import qualified Data.Foldable as Foldable
import Debug.Trace
import Data.List

data Sequence = 
  Sequence {
    offset :: Int,
    span :: Int,
    repeats :: Int
    } deriving (Show, Eq, Ord)

heap_map :: (a -> b) -> Heap.Heap a -> [b]
heap_map fn hp =
  Foldable.foldr fold_fn [] hp
  where
    fold_fn x xs = (fn x):xs

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
          | target < src_idx    = recurse next_ss next_ps
          | prefixes_match      = debug "3" (Heap.insert ext_partial next_ps, new_seqs)
          | otherwise           = debug "4" (Heap.insert new_partial next_ps, next_ss)
          where
            next_ps     = Heap.deleteMin ps
            new_partial = (src_idx+lcp, sa_idx, Sequence src_idx lcp 0)
            base_ps     = Heap.insert new_partial ps
            base        = debug "0/1" (base_ps, new_seqs)

            (target, sa_idx2, seqnce@(Sequence off sp rpt)) = Heap.minimum ps

            next_ss        = cons_partial seqnce rpt new_seqs
            ext_partial    = (src_idx+lcp, sa_idx, Sequence off sp (rpt+1))
            prefixes_match = (lcp_rmq sa_idx sa_idx2 lcp)

            show_tpl (a,b)  = "seqs=" ++ (show b) ++ "\n--" ++ (intercalate "\n--" $ heap_map show a)
            debug tag tpl   = tpl -- trace ("(" ++ tag ++ ") processed " ++ (show (src_idx, sa_idx)) ++ ": " ++ (show_tpl tpl)) tpl


sequences :: Int -> [LCP.Group] -> (Int -> Int -> Int -> Bool) -> [Sequence]
sequences srclen merged_grps lcp_rmq =
  concat $ map grp_seqs merged_grps
  where
    -- because a sentinel element is added, there cant possibly be
    -- any sequences of length n/2 or greater (sentinel element by
    -- by definition is less than all other elements, thus cannot
    -- be part of a repeating sequence)
    grp_seqs grp@(LCP.Group lcp _)
      | lcp >= (srclen `div` 2) = []
      | otherwise               = lcpGroupSequences lcp_rmq grp
