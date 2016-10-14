{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Istatd.Istatd
( ChanLike (..)
, IstatdDatum (..)
, IstatdType (..)
, mkFilterPipeline
, mkBuffer
, mkFilterPrefix
, mkFilterSuffix
, mkMonitoredBuffer
, mkSlowdownBuffer
, mkNullRecorder
, mkPrintingRecorder
, mkPrintingEncodedRecorder
, mkIstatdRecorder
)
where

import            Control.Concurrent              ( threadDelay )
import            Control.Concurrent.Async        ( async )
import            Control.Exception.Safe          ( catch )
import            Control.Exception               ( BlockedIndefinitelyOnMVar )
import            Control.Monad                   ( void
                                                  , forever
                                                  , (=<<)
                                                  , (>=>)
                                                  )
import            Control.Monad.Catch             ( MonadCatch )
import            Control.Monad.IO.Class          ( MonadIO
                                                  , liftIO
                                                  )
import            Data.Monoid                     ( (<>) )
import            Istatd.Client                   ( IstatdConfig (..)
                                                  , connect
                                                  , send
                                                  )
import            Istatd.Types                    ( IstatdDatum(..)
                                                  , IstatdType (..)
                                                  , FilterFunc
                                                  , updateKey
                                                  , getKey
                                                  , toPacket
                                                  )
import            Istatd.Tick                     ( tick )
import            Istatd.Chan.ChanLike            ( ChanLike (..) )
import            Istatd.Control.Monad            ( (<<) )

import qualified  Control.Concurrent.Chan.Unagi   as U
import qualified  Data.ByteString.Lazy.Builder    as BSLB
import qualified  Data.Time.Clock.POSIX           as POSIX




mkFilterPipeline :: (MonadIO m)
                 => ci IstatdDatum
                 -> [FilterFunc (ci IstatdDatum) m]
                 -> m (ci IstatdDatum)
mkFilterPipeline sink fs = foldr (>=>) return fs $ sink

mkFilterPrefix :: (MonadIO m, ChanLike ci ct IstatdDatum)
               => BSLB.Builder
               -> FilterFunc (ci IstatdDatum) m
mkFilterPrefix prefix = \out -> do
    (inC, outC) <- clNewZChan
    let action r@(getKey -> k) = let nk = prefix <> k
                                 in clWriteChan out $ updateKey nk r
    liftIO $ void $ async $ forever $ action =<< clReadChan outC
    return inC

mkFilterSuffix :: (MonadIO m, ChanLike ci ct IstatdDatum)
               => BSLB.Builder
               -> FilterFunc (ci IstatdDatum) m
mkFilterSuffix suffix = \out -> do
    (inC, outC) <- clNewZChan
    let action r@(getKey -> k) = let nk = k <> suffix
                                 in clWriteChan out $ updateKey nk r
    liftIO $ void $ async $ forever $ action =<< clReadChan outC
    return inC

mkBuffer :: ( MonadCatch m
            , MonadIO m
            , ChanLike ci ct IstatdDatum
            )
         => Int
         -> FilterFunc (ci IstatdDatum) m
mkBuffer size = \out -> do
    (inC, outC) <- clNewBChan size
    let action = liftIO $ void $ async $ forever $ clWriteChan out =<< clReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Buffer died" >> return ()
    return inC

mkMonitoredBuffer :: ( MonadCatch m
                     , MonadIO m
                     , ChanLike ci ct IstatdDatum
                     )
                  => BSLB.Builder
                  -> Int
                  -> FilterFunc (ci IstatdDatum) m
mkMonitoredBuffer name size = \out -> do
    (inC, outC) <- clNewBChan size
    let action = liftIO $ void $ async $ forever $ clWriteChan out =<< (clReadChan outC)
    ticker <- tick 1
    let recordAction =
            let channelAction =
                    let datum len t = IstatdDatum Gauge name t (fromIntegral len)
                        writeAction len = clWriteChan out . datum len =<< POSIX.getPOSIXTime
                    in U.readChan ticker >> (writeAction =<< clInChanLen inC)
            in liftIO $ void $ async $ forever $ channelAction
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "MonitoredBuffer died" >> return ()
    recordAction
    return inC

mkSlowdownBuffer :: ( MonadCatch m
                    , MonadIO m
                    , ChanLike ci ct IstatdDatum
                    )
                 => Int
                 -> Int
                 -> FilterFunc (ci IstatdDatum) m
mkSlowdownBuffer time size = \out -> do
    (inC, outC) <- clNewBChan size
    let action = liftIO $ void $ async $ forever $ clWriteChan out =<< (clReadChan outC << threadDelay time)
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "MonitoredBuffer died" >> return ()
    return inC

mkNullRecorder :: ( MonadCatch m
                  , MonadIO m
                  , ChanLike ci ct IstatdDatum
                  )
               => m (ci IstatdDatum)
mkNullRecorder = do
    (inC, outC) <- clNewZChan
    let action = liftIO $ void $ async $ forever $ void $ clReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Recorder died" >> return ()
    return inC

mkPrintingRecorder :: ( MonadCatch m
                      , MonadIO m
                      , ChanLike ci ct IstatdDatum
                      )
                   => m (ci IstatdDatum)
mkPrintingRecorder = do
    (inC, outC) <- clNewZChan
    let action = liftIO $ void $ async $ forever $ putStrLn . show =<< clReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Recorder died" >> return ()
    return inC

mkPrintingEncodedRecorder :: ( MonadCatch m
                             , MonadIO m
                             , ChanLike ci ct IstatdDatum
                             )
                          => m (ci IstatdDatum)
mkPrintingEncodedRecorder = do
    (inC, outC) <- clNewZChan
    let action = liftIO $ void $ async $ forever $ putStrLn . show . toPacket =<< clReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Recorder died" >> return ()
    return inC

mkIstatdRecorder :: ( MonadCatch m
                    , MonadIO m
                    , ChanLike ci ct IstatdDatum
                    )
                 => IstatdConfig
                 -> m (ci IstatdDatum)
mkIstatdRecorder config = do
    (inC, outC) <- clNewZChan
    connection <- connect config
    let send' p = send connection [p]
    let action = liftIO $ void $ async $ forever $ send' =<< clReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Recorder died" >> return ()
    return inC



putStrLnIO :: (MonadIO m)
           => String -> m ()
putStrLnIO = liftIO . putStrLn

