module Data.CycleRoll.Util (
  mapHeap,
  listFromHeap
  ) where

import qualified Data.Heap as Heap
import qualified Data.Foldable as Foldable

mapHeap :: (a -> b) -> Heap.Heap a -> [b]
mapHeap fn hp =
  Foldable.foldr fold_fn [] hp
  where
    fold_fn x xs = (fn x):xs

listFromHeap :: Heap.Heap a -> [a]
listFromHeap = mapHeap id
