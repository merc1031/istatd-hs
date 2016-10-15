{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Istatd.Chan.ChanLike where

import          Control.Monad.IO.Class      ( MonadIO )
import          Control.Exception           ( Exception )
import          Data.Typeable               ( Typeable )

data ChannelException = ChannelBlockedException
                      deriving (Show, Typeable)

instance Exception ChannelException

class ChanLike cit cot a | cit a -> cot a, cot a -> cit a where
  clNewZChan :: MonadIO m => m (cit a, cot a)
  clNewBChan :: MonadIO m => Int -> m (cit a, cot a)

  clWriteChan :: MonadIO m => (cit a) -> a -> m ()
  clReadChan :: MonadIO m => (cot a) -> m a

  clInChanLen :: MonadIO m => (cit a) -> m Int
  clOutChanLen :: MonadIO m => (cot a) -> m Int
