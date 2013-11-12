module Data.CycleRoll.Internal.RSequence (
  Root(..),
  length
  ) where

import Prelude hiding (length)

import qualified Data.CycleRoll.Internal.RSequence.Node as Node

data Root = Root {
  offset :: Int,
  root :: Node.Node
  } deriving (Show, Eq, Ord)

length :: Root -> Int
length (Root _ rt) = Node.length rt
