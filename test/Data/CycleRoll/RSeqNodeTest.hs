module Data.CycleRoll.RSeqNodeTest (
  group
  ) where

import Prelude hiding (length)

import Data.CycleRoll.RSeqNode
import Test.Utils

import qualified Data.Sequence as DSeq

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

sl = DSeq.fromList

group = 
  testGroup "RSeqNode tests" [
    node_group
    ]

node_group = 
  testGroup "node" [
    testProperty "len"          prop_len,
    testProperty "txfm_eq_len"  prop_txfm_eq_len,
    testCase     "txfm_test1"   txfm_test1
    ]
  where
    prop_len :: RSeqNodeWSz -> Bool
    prop_len (RSeqNodeWSz sz rsnode) = 
      sz == length rsnode

    prop_txfm_eq_len :: RSeqNodeWSz -> Bool
    prop_txfm_eq_len (RSeqNodeWSz sz rsnode) = 
      let
        fn _ _ _ = TxfmConst ()
        result   = transform fn () rsnode
      in sz == fst result

    txfm_test1 =
      let 
        mp = 
          [
            0,
              0,
              4,
                4,
                  4,                
                  4+4,
                (8+4)+8,
              4+5*(8*2 + 6*3),
              174+3,
                174+3,
                177+5*4,
                  177 + 5*4,
                  197 + 3*2
            ]

        rseq = 
          Node 0 $ sl [        -- 0
            Leaf 2 1,               -- 0
            Node 4 $ sl [      -- 4
              Node 1 $ sl [    -- 4
                Leaf 4 0,           -- 4
                Leaf 1 3            -- 8
                ],
              Leaf 6 2              -- (8+4)+8
              ], 
            Leaf 3 0,               -- 4+5*(8*2 + 6*3)
            Node 1 $ sl [      -- 174+3
              Leaf 5 3,             -- 174+3
              Node 3 $ sl [    -- 177 + 5*4
                Leaf 3 1,           -- 177+5*4
                Leaf 2 3            -- 197+3*2
                ] 
              ] 
            ] 

        fn off (idx, bl) (Leaf _ _) = 
          TxfmConst (idx+1, bl && (mp !! idx) == off)
        fn off (idx, bl) _ = 
          TxfmPair (idx+1, bl && (mp !! idx) == off) id
      in (13, True) @=? (snd $ transform fn (0, True) rseq)

