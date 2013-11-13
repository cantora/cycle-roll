module Data.CycleRoll.RSequence (
  Root(..),
  display,
  displayList
  ) where

import Data.CycleRoll.Internal.RSequence (Root(..))
import qualified Data.CycleRoll.Internal.RSequence as RSequence
import Data.CycleRoll.Internal.Prepare (Input(..))

import qualified Data.Vector.Unboxed as UV

display :: (Show a, UV.Unbox a) => Input a -> Root -> String
display (ConstructInput input) =
  RSequence.display input

displayList :: (Show a, UV.Unbox a) => Input a -> [Root] -> String
displayList (ConstructInput input) =
  RSequence.displayList input
