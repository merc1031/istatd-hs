module Istatd.Chan where

import            Control.Arrow                           ( (***) )
import            Control.Monad                           ( void )
import            Control.Monad.IO.Class                  ( MonadIO
                                                          , liftIO
                                                          )
import            Data.IORef                              ( atomicModifyIORef
                                                          , newIORef
                                                          )

import qualified  Control.Concurrent.Chan.Unagi           as U
import qualified  Control.Concurrent.Chan.Unagi.Bounded   as BU

data InChanI a = ZInChan (U.InChan a)
               | BInChan (IORef Int) (BU.InChan a)

data OutChanI a = ZOutChan (U.OutChan a)
                | BOutChan (IORef Int) (BU.OutChan a)

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
