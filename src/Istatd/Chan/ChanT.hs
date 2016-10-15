{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Istatd.Chan.ChanT where

import            Control.Arrow                           ( (***) )
import            Control.Concurrent.STM                  ( TVar
                                                          , modifyTVar'
                                                          , atomically
                                                          , newTVarIO
                                                          , readTVarIO
                                                          )
import            Control.Exception                       ( BlockedIndefinitelyOnMVar )
import            Control.Exception.Safe                  ( handle
                                                          , throwM
                                                          )
import            Control.Monad                           ( void )
import            Control.Monad.Catch                     ( MonadCatch )
import            Control.Monad.IO.Class                  ( MonadIO
                                                          , liftIO
                                                          )
import            Control.DeepSeq                         ( NFData (..) )
import            Istatd.Chan.ChanLike                    ( ChanLike (..)
                                                          , ChannelException (..)
                                                          )

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
               | BInChan (TVar Int) (BU.InChan a)

data OutChanI a = ZOutChan (U.OutChan a)
                | BOutChan (TVar Int) (BU.OutChan a)

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
  liftIO $ handleBlocked $ U.writeChan uchan r
iWriteChan (BInChan c buchan) r =
  liftIO $ handleBlocked $ BU.writeChan buchan r >> (void $ atomically $ modifyTVar' c (\c' -> c' + 1))

iReadChan :: (MonadIO m)
          => OutChanI a
          -> m a
iReadChan (ZOutChan uchan) =
  liftIO $ handleBlocked $ U.readChan uchan
iReadChan (BOutChan c buchan) =
  liftIO $ handleBlocked $ BU.readChan buchan >>= \r -> (void $ atomically $ modifyTVar' c (\c' -> c' - 1)) >> return r

iOutChanLen :: (MonadIO m)
            => OutChanI a
            -> m Int
iOutChanLen (ZOutChan {}) =
  return 0
iOutChanLen (BOutChan c _) =
  liftIO $ readTVarIO c

iInChanLen :: (MonadIO m)
           => InChanI a
           -> m Int
iInChanLen (ZInChan {}) =
  return 0
iInChanLen (BInChan c _) =
  liftIO $ readTVarIO c

newZChan :: (MonadIO m)
         => m (InChanI a, OutChanI a)
newZChan =
  (ZInChan *** ZOutChan) <$> liftIO U.newChan

newBChan :: (MonadIO m)
         => Int
         -> m (InChanI a, OutChanI a)
newBChan size = do
  c <- liftIO $ newTVarIO 0
  (BInChan c *** BOutChan c) <$> (liftIO $ BU.newChan size)

handleBlocked :: (MonadCatch m)
              => m a
              -> m a
handleBlocked a = handle (\(_ :: BlockedIndefinitelyOnMVar) -> throwM ChannelBlockedException) a
