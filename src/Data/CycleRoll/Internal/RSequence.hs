module Data.CycleRoll.Internal.RSequence (
  Root(..),
  length,
  display,
  displayList
  ) where

import Prelude hiding (length)

import qualified Data.CycleRoll.Internal.RSequence.Node as Node

import qualified Data.Vector.Unboxed as UV
import Data.List (intercalate)

data Root = Root {
  offset :: Int,
  root :: Node.Node
  } deriving (Show, Eq, Ord)

length :: Root -> Int
length (Root _ rt) = Node.length rt

display :: (Show a, UV.Unbox a) => UV.Vector a -> Root -> String
display input (Root off node) = Node.display off input node

displayList :: (Show a, UV.Unbox a) => UV.Vector a -> [Root] -> String
displayList input xs = intercalate ", " $ map (display input) xs
