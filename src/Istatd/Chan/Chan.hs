{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Istatd.Chan.Chan where

import            Control.Arrow                           ( (***) )
import            Control.Monad                           ( void )
import            Control.Monad.IO.Class                  ( MonadIO
                                                          , liftIO
                                                          )
import            Data.IORef                              ( IORef
                                                          , atomicModifyIORef'
                                                          , newIORef
                                                          )
import            Control.DeepSeq                         ( NFData (..) )
import            Istatd.Chan.ChanLike                    ( ChanLike (..) )

import qualified  Control.Concurrent.Chan.Unagi           as U
import qualified  Control.Concurrent.Chan.Unagi.Bounded   as BU


instance ChanLike InChanI OutChanI a where
  clNewZChan = newZChan
  clNewBChan = newBChan
  clWriteChan = iWriteChan
  clReadChan = iReadChan
  clInChanLen = iInChanLen
  clOutChanLen = iOutChanLen

data InChanI a = ZInChan (U.InChan a)
               | BInChan (IORef Int) (BU.InChan a)

data OutChanI a = ZOutChan (U.OutChan a)
                | BOutChan (IORef Int) (BU.OutChan a)

instance NFData (InChanI a) where
  rnf (ZInChan !_chan) = ()
  rnf (BInChan !_ref !_chan) = ()

instance NFData (OutChanI a) where
  rnf (ZOutChan !_chan) = ()
  rnf (BOutChan !_ref !_chan) = ()

iWriteChan :: MonadIO m
           => InChanI a
           -> a
           -> m ()
iWriteChan (ZInChan uchan) r =
  liftIO $ U.writeChan uchan r
iWriteChan (BInChan c buchan) r =
  liftIO $ BU.writeChan buchan r >> (void $ atomicModifyIORef' c (\c' -> (c' + 1, ())))

iReadChan :: (MonadIO m)
          => OutChanI a
          -> m a
iReadChan (ZOutChan uchan) =
  liftIO $ U.readChan uchan
iReadChan (BOutChan c buchan) =
  liftIO $ BU.readChan buchan >>= \r -> (void $ atomicModifyIORef' c (\c' -> (c' - 1, ()))) >> return r

iOutChanLen :: (MonadIO m)
            => OutChanI a
            -> m Int
iOutChanLen (ZOutChan {}) =
  return 0
iOutChanLen (BOutChan c _) =
  liftIO $ atomicModifyIORef' c (\c' -> (c', c'))

iInChanLen :: (MonadIO m)
           => InChanI a
           -> m Int
iInChanLen (ZInChan {}) =
  return 0
iInChanLen (BInChan c _) =
  liftIO $ atomicModifyIORef' c (\c' -> (c', c'))

newZChan :: (MonadIO m)
         => m (InChanI a, OutChanI a)
newZChan =
  (ZInChan *** ZOutChan) <$> liftIO U.newChan

newBChan :: (MonadIO m)
         => Int
         -> m (InChanI a, OutChanI a)
newBChan size = do
  c <- liftIO $ newIORef 0
  (BInChan c *** BOutChan c) <$> (liftIO $ BU.newChan size)
