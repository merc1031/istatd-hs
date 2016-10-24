{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Istatd.ChanLike where

import          Control.Monad.IO.Class      ( MonadIO )

class ChanLike cit cot (as :: [* -> *]) | cit a -> cot a, cot a -> cit a where
  clNewZChan :: MonadIO m => m (cit a, cot a)
  clNewBChan :: MonadIO m => Int -> m (cit a, cot a)

  clWriteChan :: MonadIO m => (cit a) -> a -> m ()
  clReadChan :: MonadIO m => (cot a) -> m a

  clInChanLen :: MonadIO m => (cit a) -> m Int
  clOutChanLen :: MonadIO m => (cot a) -> m Int
