module Main (
    main
 ) where

import Test.Framework
import Test.Framework.Options
import Test.Framework.Runners.Options
import Data.Monoid (mempty)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Data.CycleRoll.Internal.LCPTest as LCPTest
import Data.CycleRoll.Internal.SuffixArrayTest as SATest
import Data.CycleRoll.Internal.RSequence.NodeTest as RSeqNodeTest
import Data.CycleRoll.Internal.SubSeqTest as SubSeqTest
import Data.CycleRoll.Internal.PrepareTest as PrepareTest
import Data.CycleRoll.Test as CycleRollTest

main :: IO ()
main = do
  let empty_test_opts = mempty :: TestOptions
  let my_test_opts = empty_test_opts {
    --topt_maximum_test_size       = Just 99,
    topt_maximum_generated_tests = Just 9999
  }
  let empty_runner_opts = mempty :: RunnerOptions
  let my_runner_opts = empty_runner_opts {
    ropt_test_options = Just my_test_opts
  }

  defaultMainWithOpts tests my_runner_opts

tests :: [Test]
tests = [
  SATest.group,
  LCPTest.group,
  RSeqNodeTest.group,
  SubSeqTest.group,
  PrepareTest.group,
  CycleRollTest.group
  ]
