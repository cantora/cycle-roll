module Data.CycleRoll (
  roll
  ) where

import Data.CycleRoll.Internal.Prepare (Input(..))
import qualified Data.CycleRoll.Internal.Algorithm as Algorithm
import qualified Data.CycleRoll.RSequence as RSequence
import qualified Data.CycleRoll.Internal.Sequence as Seq
import qualified Data.CycleRoll.Internal.SuffixArray as SA
import qualified Data.CycleRoll.Internal.LCP as LCP

import qualified Data.Vector.Unboxed as UV
import Data.Ix

roll :: (Ix a, Bounded a, UV.Unbox a) => Input a -> [RSequence.Root]
roll (ConstructInput input) =
  Algorithm.roll i_len $ Seq.sequences i_len groups rmq
  where
    i_len  = UV.length input
    sa     = SA.make input
    lcps   = LCP.array input sa
    rmq    = LCP.rmq lcps
    groups = LCP.mergedGroups $ LCP.groups sa lcps
