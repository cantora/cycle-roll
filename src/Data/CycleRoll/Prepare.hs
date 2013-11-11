module Data.CycleRoll.Prepare (
  fromProducer,
  fromList
  ) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import Pipes
import Control.Monad.Primitive
import Control.Applicative
import Control.Monad.ST

noSentinelErr :: String
noSentinelErr = 
  "expected to receive a sentinel terminator before end of pipe data"

appendSentinel :: (Monad m, Ord a) => a -> Producer a m () -> Producer a m ()
appendSentinel sentinel prod = do
  for prod $ \x -> yield x
  yield sentinel

fromProducer :: (PrimMonad m, Ord a, UV.Unbox a) => 
  a -> Producer a m () -> m (UV.Vector a)
fromProducer sentinel producer = 
  runEffect $ (error noSentinelErr <$ real_prod) >-> consume
  where
    real_prod = appendSentinel sentinel producer
    loop n vec = do
      x <- await
      lift $ MV.write vec n x
      if (x <= sentinel)
        then lift $ UV.unsafeFreeze vec
        else (lift $ MV.unsafeGrow vec 1) >>= loop (n+1)

    consume = (lift $ MV.new 1) >>= loop 0

fromList :: (UV.Unbox a, Ord a) => a -> [a] -> UV.Vector a
fromList sentinel xs =
  runST $ fromProducer sentinel (each xs)
