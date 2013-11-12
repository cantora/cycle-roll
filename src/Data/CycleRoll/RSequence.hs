module Data.CycleRoll.RSequence (
  Root(..),
  display,
  displayList
  ) where

import qualified Data.CycleRoll.Internal.RSequence.Node as Node
import Data.CycleRoll.Internal.RSequence (Root(..))

import qualified Data.Vector.Unboxed as UV
import Data.List

display :: (Show a, UV.Unbox a) => UV.Vector a -> Root -> String
display input (Root off node) = Node.display off input node

displayList :: (Show a, UV.Unbox a) => UV.Vector a -> [Root] -> String
displayList input xs = intercalate ", " $ map (display input) xs
