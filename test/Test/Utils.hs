module Test.Utils (
  makeRSeqLeaf,
  makeRSeqLeafGen,
  RSeqNodeWSz(..)
  ) where

import Data.CycleRoll.RSeqNode as RSeq

import Control.Monad
import Test.QuickCheck


makeRSeqLeaf :: Int -> Int -> RSeq.Node
makeRSeqLeaf n m =
  Leaf q $ (n `div` q) - 1
  where
    find_divisor n m 
      | m > n           = error "m must be <= n"
      | n `mod` m == 0  = m
      | otherwise       = find_divisor n $ m+1
    q = find_divisor n m

makeRSeqLeafGen :: Int -> Gen RSeq.Node
makeRSeqLeafGen n = liftM (makeRSeqLeaf n) $ choose (1, n)

data RSeqNodeWSz = 
  RSeqNodeWSz Int RSeq.Node deriving (Show, Eq, Ord)

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
            return $ RSeq.Node rpt $ extra:subs
          else 
            return $ case subs of 
              ((RSeq.Node subs_rpt subs_subs):[])
                -> RSeq.Node ((subs_rpt+1)*(rpt+1) - 1) subs_subs
              ((Leaf subs_sp subs_rpt):[])
                -> Leaf subs_sp $ (subs_rpt+1)*(rpt+1) - 1
              _ -> RSeq.Node rpt subs

      make 0 = fail "cant make rseqnode with 0 size"
      make 1 = makeRSeqLeafGen 1
      make n = frequency [(1, makeRSeqLeafGen n), (1, make_node n)]
