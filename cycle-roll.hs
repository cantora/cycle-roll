module Main where

import qualified Data.CycleRoll as CR
import qualified Data.CycleRoll.Prepare as Prepare
import qualified Data.CycleRoll.RSequence as RSequence

import System.Environment
import Data.List
import qualified Data.Vector.Unboxed as UV

main = do
  input_str <- getContents
  let input = Prepare.fromList '\00' input_str
  --putStrLn $ show input
  let result = CR.roll input
  putStrLn $ RSequence.displayList input result
