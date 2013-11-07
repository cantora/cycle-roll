module Data.CycleRoll.RSeqNode (
  Node(..),
  length,
  size,
  transform,
  TxfmDir(..),
  foldLeaves,
  display
  ) where

import Prelude hiding (length, foldl)

import qualified Data.CycleRoll.Sequence as S

import Data.Foldable (foldl)
import qualified Data.Vector.Unboxed as UV
import qualified Data.List as List
import qualified Data.Sequence as DSeq

data Node =
  Leaf { span :: Int, repeat :: Int }
  | Node { repeat :: Int, subseqs :: DSeq.Seq Node }
  deriving (Show, Eq, Ord)

data TxfmDir a = TxfmConst a           -- use 'a' value as new accumulator, dont process children
                 | TxfmCB (a -> a)     -- pass processed children to function, 
                                       --   process children using default accumulator
                 | TxfmPair a (a -> a) -- pass processed children to function, process
                                       --   children using provided accumulator

transform ::
  ( Int ->         -- current offset
    a ->           -- accumulator
    Node ->        -- current node
    TxfmDir a) ->  -- directive which specifies how to process children
  a ->           -- accumulator
  Node ->        -- root node
  (Int, a)       -- final offset of node, accumulator result
transform fn base_acc rsnode =
  visit 0 0 base_acc rsnode
  where
    recurse off (sz, acc) n = visit off sz acc n

    visit off sz acc r@(Leaf sp rpt) =
      (sz + (S.length' sp rpt), result tdir)
      where
        tdir = fn (off+sz) acc r
        result (TxfmConst new_acc) = new_acc
        result (TxfmCB cb)         = cb base_acc
        result (TxfmPair base cb)  = cb base

    visit off sz acc r@(Node rpt subs) =
      (sz + (S.length' rlen rpt), acc_result)
      where
        tdir = fn (off+sz) acc r
        (rlen, acc_result) = result tdir
        ch_result cb base =
          (\(rl, a) -> (rl, cb a)) $ foldl (recurse (off+sz)) (0, base) subs
        result (TxfmConst new_acc)    = (fst $ ch_result id base_acc, new_acc)
        result (TxfmCB cb)            = ch_result cb base_acc
        result (TxfmPair new_base cb) = ch_result cb new_base

foldLeaves :: 
  ( Int ->       -- current offset
    a ->         -- accumulator
    Node ->      -- current node
    a) ->        -- returns new accumulator
  a ->         -- accumulator
  Node ->      -- root node
  (Int, a)     -- length of node, accumulator result
foldLeaves fn base rsnode =
  transform vfn base rsnode
  where
    vfn curr_off acc r@(Leaf _ _) = TxfmConst $ fn curr_off acc r
    vfn _        acc _            = TxfmPair acc id


--total length of all the leaf nodes combined
length :: Node -> Int
length = 
  fst . (foldLeaves (\_ _ _ -> ()) ())

--number of leaf nodes in the recursive sequence
size :: Node -> Int
size = 
  snd . (foldLeaves (\_ n _ -> n+1) 0)

display :: (Show a, UV.Unbox a) => Int -> UV.Vector a -> Node -> String
display base_off input rsnode = 
  head . snd $ transform fn [] rsnode
  where
    slice off sp = 
      show $ UV.toList $ UV.slice (base_off+off) sp input
    show_rpt rpt
      | rpt > 0   = "*" ++ (show (rpt+1))
      | otherwise = ""

    fn off sibs (Leaf sp rpt) =
      TxfmConst $ sibs ++ [(slice off sp) ++ (show_rpt rpt)]

    fn _ sibs (Node rpt _) =
      TxfmCB $ \ch -> 
        sibs ++ [
          "(" ++ 
          (List.intercalate ", " ch) ++
          ")" ++ 
          (show_rpt rpt)
          ]
