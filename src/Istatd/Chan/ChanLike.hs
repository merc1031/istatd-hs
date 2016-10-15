{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Istatd.Chan.ChanLike where

import          Control.Monad.IO.Class      ( MonadIO )
import          Control.Exception           ( Exception )
import          Data.Typeable               ( Typeable )

data ChannelException = ChannelBlockedException
                      deriving (Show, Typeable)

instance Exception ChannelException

-- | `ChanLike` implements an interface for channels that want to talk about
-- in and out sides. They are associated via a cyclical functional dependency.
-- In this way a function using an in(`cit`) or out(`cot`) channel of the correct `a`
-- may bring the type of the associated other end into scope.
-- This interface allows the generalized code to not worry about the type of the
-- specific channel or its underlying concurrency implementation.
class ChanLike cit cot a | cit a -> cot a, cot a -> cit a where
  newZChan :: MonadIO m => m (cit a, cot a)
  newBChan :: MonadIO m => Int -> m (cit a, cot a)

  writeChan :: MonadIO m => (cit a) -> a -> m ()
  readChan :: MonadIO m => (cot a) -> m a

  inChanLen :: MonadIO m => (cit a) -> m Int
  outChanLen :: MonadIO m => (cot a) -> m Int
