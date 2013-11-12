module Data.CycleRoll.Prepare (
  Input,
  fromList
  ) where

import qualified Data.CycleRoll.Internal.Prepare as Prepare
import Data.CycleRoll.Internal.Prepare (Input)

import qualified Data.Vector.Unboxed as UV

fromList :: (UV.Unbox a, Ord a) => a -> [a] -> Input a
fromList sentinel =
  Prepare.ConstructInput . (Prepare.fromList sentinel)
