{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Istatd.Chan.ChanT where

import            Control.Arrow                           ( (***) )
import            Control.Concurrent.STM                  ( TVar
                                                          , modifyTVar'
                                                          , atomically
                                                          , newTVarIO
                                                          , readTVarIO
                                                          )
import            Control.DeepSeq                         ( NFData (..) )
import            Control.Exception                       ( BlockedIndefinitelyOnMVar )
import            Control.Exception.Safe                  ( handle
                                                          , throwM
                                                          )
import            Control.Monad                           ( void )
import            Control.Monad.Catch                     ( MonadCatch )
import            Control.Monad.IO.Class                  ( MonadIO
                                                          , liftIO
                                                          )
import            Istatd.Chan.ChanLike                    ( ChanLike (..)
                                                          , ChannelException (..)
                                                          )
import            Istatd.Simplicity

import qualified  Control.Concurrent.Chan.Unagi           as U
import qualified  Control.Concurrent.Chan.Unagi.Bounded   as BU

instance ChanLike InChanI OutChanI (as :: [*]) where
  newZChan   = iNewZChan
  newBChan   = iNewBChan
  writeChan  = iWriteChan
  readChanM  = iReadChanM
  writeRaw   = iWriteRaw
  readRaw    = iReadRaw
  inChanLen  = iInChanLen
  outChanLen = iOutChanLen

data InChanI a =
    ZInChan (U.InChan a)
  | BInChan (TVar Int) (BU.InChan a)

data OutChanI a =
    ZOutChan (U.OutChan a)
  | BOutChan (TVar Int) (BU.OutChan a)

instance NFData (InChanI a) where
  rnf (ZInChan !_chan) = ()
  rnf (BInChan !_ref !_chan) = ()

instance NFData (OutChanI a) where
  rnf (ZOutChan !_chan) = ()
  rnf (BOutChan !_ref !_chan) = ()

iWriteRaw
  :: (MonadIO m)
  => InChanI (Summed as)
  -> Summed as
  -> m ()
iWriteRaw (ZInChan uchan) r =
  liftIO $ handleBlocked $ U.writeChan uchan $ r
iWriteRaw (BInChan c buchan) r =
  liftIO $ handleBlocked $ BU.writeChan buchan (r) >> (modLen c (+))

iReadRaw
  :: (MonadIO m)
  => OutChanI (Summed as)
  -> m (Summed as)
iReadRaw (ZOutChan uchan) =
  (liftIO $ handleBlocked $ U.readChan uchan)
iReadRaw (BOutChan c buchan) =
  (liftIO $ handleBlocked $ BU.readChan buchan >>= \r -> (modLen c (-)) >> return r)

iWriteChan
  :: ( MonadIO m
     , a :<: as
     )
  => InChanI (Summed as)
  -> a
  -> m ()
iWriteChan (ZInChan uchan) r =
  liftIO $ handleBlocked $ U.writeChan uchan $ inj $ r
iWriteChan (BInChan c buchan) r =
  liftIO $ handleBlocked $ BU.writeChan buchan (inj $ r) >> modLen c (+)

iReadChanM
  :: ( MonadIO m
     , a :<: as
     )
  => OutChanI (Summed as)
  -> m (Maybe a)
iReadChanM (ZOutChan uchan) =
  (outj) <$> (liftIO $ handleBlocked $ U.readChan uchan)
iReadChanM (BOutChan c buchan) =
  (outj) <$> (liftIO $ handleBlocked $ BU.readChan buchan >>= \r -> (modLen c (-)) >> return r)

iOutChanLen
  :: (MonadIO m)
  => OutChanI a
  -> m Int
iOutChanLen (ZOutChan {}) =
  return 0
iOutChanLen (BOutChan c _) =
  liftIO $ getLen c

iInChanLen
  :: (MonadIO m)
  => InChanI a
  -> m Int
iInChanLen (ZInChan {}) =
  return 0
iInChanLen (BInChan c _) =
  liftIO $ getLen c

iNewZChan
  :: (MonadIO m)
  => m (InChanI a, OutChanI a)
iNewZChan =
  (ZInChan *** ZOutChan) <$> liftIO U.newChan

iNewBChan
  :: (MonadIO m)
  => Int
  -> m (InChanI a, OutChanI a)
iNewBChan size = do
  c <- liftIO $ newTVarIO 0
  (BInChan c *** BOutChan c) <$> (liftIO $ BU.newChan size)

modLen
  :: TVar Int
  -> (Int -> Int -> Int)
  -> IO ()
modLen c op =
  void $ atomically $ modifyTVar' c (\c' -> c' `op` 1)

getLen
  :: TVar Int
  -> IO Int
getLen c =
  readTVarIO c

handleBlocked
  :: (MonadCatch m)
  => m a
  -> m a
handleBlocked a =
  handle (\(_ :: BlockedIndefinitelyOnMVar) -> throwM ChannelBlockedException) a

data Ctr = Ctr
data Ctr2 = Ctr2

test :: (MonadIO m)
     => m ()
test = do
  (inC, outC) <- newZChan @InChanI @OutChanI @'[Ctr, Ctr2]
  writeChan inC Ctr
  ctr <- readChan outC
  case ctr of
    Ctr -> liftIO $ putStrLn "Read ctr"
  writeChan inC Ctr2
  ctr2 <- readChan outC
  case ctr2 of
    Ctr2 -> liftIO $ putStrLn "Read ctr2"
  return ()
