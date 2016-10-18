{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Istatd.Istatd
( ChanLike (..)
, IstatdDatum (..)
, IstatdType (..)
, DifferenceCounter (..)
, ChannelException (..)
, FilterFunc
, FilterFuncT
, mkFilterPipeline
, mkFilterPipelineWithSource
, mkBuffer
, mkFilterPrefix
, mkFilterSuffix
, mkMonitoredBuffer
, mkSlowdownBuffer
, mkNullRecorder
, mkPrintingRecorder
, mkPrintingEncodedRecorder
, mkIstatdRecorder
, mkPipeRecorder
, mkFilterDifference
, mkDifferenceState
, mkPercentileFilter
, mkPercentile
, mkPercentiles
)
where

import            Control.Concurrent              ( threadDelay )
import            Control.Concurrent.Async        ( async )
import            Control.Exception.Safe          ( catch )
import            Control.Monad                   ( void
                                                  , forever
                                                  , forM_
                                                  , (=<<)
                                                  , (>=>)
                                                  , (<=<)
                                                  )
import            Control.Monad.Catch             ( MonadCatch )
import            Control.Monad.IO.Class          ( MonadIO
                                                  , liftIO
                                                  )
import            Data.Monoid                     ( (<>) )
import            Istatd.Chan.ChanLike            ( ChanLike (..)
                                                  , ChannelException (..)
                                                  )
import            Istatd.Client                   ( IstatdConfig (..)
                                                  , connect
                                                  , send
                                                  )
import            Istatd.Control.Monad            ( (<<) )
import            Istatd.Datum.DifferenceCounter  ( DifferenceCounter (..)
                                                  , DifferenceState (..)
                                                  , mkDifferenceState
                                                  , computeDifferenceCounter
                                                  )
import            Istatd.Datum.Percentile         ( Percentile
                                                  , mkPercentile
                                                  , mkPercentiles
                                                  , mkPercentileState
                                                  , clearPercentiles
                                                  , computePercentiles
                                                  , addPercentile
                                                  )
import            Istatd.Tick                     ( tick )
import            Istatd.Types                    ( IstatdDatum(..)
                                                  , IstatdType (..)
                                                  , FilterFunc
                                                  , FilterFuncT
                                                  , updateKey
                                                  , getKey
                                                  , toPacket
                                                  )

import qualified  Control.Concurrent.Chan.Unagi   as U
import qualified  Data.ByteString.Lazy.Builder    as BSLB
import qualified  Data.Time.Clock.POSIX           as POSIX



mkFilterPipelineWithSource :: (MonadIO m)
                           => ci IstatdDatum
                           -> FilterFuncT (ci IstatdDatum) (ci' a) m
                           -> [FilterFunc (ci IstatdDatum) m]
                           -> m (ci' a)
mkFilterPipelineWithSource sink source fs = source <=< foldr (>=>) return fs $ sink


mkFilterPipeline :: (MonadIO m)
                 => ci IstatdDatum
                 -> [FilterFunc (ci IstatdDatum) m]
                 -> m (ci IstatdDatum)
mkFilterPipeline sink fs = foldr (>=>) return fs $ sink

mkFilterDifference :: ( MonadIO m
                      , ChanLike ci co IstatdDatum
                      , ChanLike ci' co' DifferenceCounter
                      )
                   => DifferenceState
                   -> FilterFuncT (ci IstatdDatum) (ci' DifferenceCounter) m
mkFilterDifference dstate = \out -> do
  (inC, outC) <- newZChan
  let action dc = writeChan out =<< computeDifferenceCounter dstate dc
  go $ forever $ action =<< readChan outC
  return inC

mkFilterPrefix :: ( MonadIO m
                  , ChanLike ci co IstatdDatum
                  )
               => BSLB.Builder
               -> FilterFunc (ci IstatdDatum) m
mkFilterPrefix prefix = \out -> do
  (inC, outC) <- newZChan
  let action r@(getKey -> k) =
        let nk = prefix <> k
        in writeChan out $ updateKey nk r
  go $ forever $ action =<< readChan outC
  return inC

mkFilterSuffix :: ( MonadIO m
                  , ChanLike ci co IstatdDatum
                  )
               => BSLB.Builder
               -> FilterFunc (ci IstatdDatum) m
mkFilterSuffix suffix = \out -> do
  (inC, outC) <- newZChan
  let action r@(getKey -> k) =
        let nk = k <> suffix
        in writeChan out $ updateKey nk r
  go $ forever $ action =<< readChan outC
  return inC

mkBuffer :: ( MonadCatch m
            , MonadIO m
            , ChanLike ci co IstatdDatum
            )
         => Int
         -> FilterFunc (ci IstatdDatum) m
mkBuffer size = \out -> do
  (inC, outC) <- newBChan size
  let action = go $ forever $ writeChan out =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Buffer died" >> return ()
  return inC

mkMonitoredBuffer :: ( MonadCatch m
                     , MonadIO m
                     , ChanLike ci co IstatdDatum
                     )
                  => BSLB.Builder
                  -> Int
                  -> FilterFunc (ci IstatdDatum) m
mkMonitoredBuffer name size = \out -> do
  (inC, outC) <- newBChan size
  let action = go $ forever $ writeChan out =<< (readChan outC)
  ticker <- tick 1
  let recordAction =
        let channelAction =
              let datum len t = IstatdDatum Gauge name t (fromIntegral len)
                  writeAction len = writeChan out . datum len =<< POSIX.getPOSIXTime
              in U.readChan ticker >> (writeAction =<< inChanLen inC)
        in go $ forever $ channelAction
  action `catch` \(_ :: ChannelException) -> putStrLnIO "MonitoredBuffer died" >> return ()
  recordAction
  return inC

mkSlowdownBuffer :: ( MonadCatch m
                    , MonadIO m
                    , ChanLike ci co IstatdDatum
                    )
                 => Int
                 -> Int
                 -> FilterFunc (ci IstatdDatum) m
mkSlowdownBuffer time size = \out -> do
  (inC, outC) <- newBChan size
  let action = go $ forever $ writeChan out =<< (readChan outC << threadDelay time)
  action `catch` \(_ :: ChannelException) -> putStrLnIO "MonitoredBuffer died" >> return ()
  return inC

mkPercentileFilter :: ( MonadCatch m
                      , MonadIO m
                      , ChanLike ci co IstatdDatum
                      )
                   => Int
                   -> [Percentile]
                   -> FilterFunc (ci IstatdDatum) m
mkPercentileFilter seconds percentiles = \out -> do
  (inC, outC) <- newZChan
  pstate <- mkPercentileState
  let collectPercentile d@(IstatdDatum Gauge k _ v) = addPercentile pstate (BSLB.toLazyByteString k) v >> (return $ updateKey (k <> BSLB.lazyByteString ".raw") d)
      collectPercentile d = return d
      action = go $ forever $ writeChan out =<< collectPercentile =<< (readChan outC)
  ticker <- tick seconds
  let recordAction =
        let channelAction =
              let writeAction = computePercentiles pstate percentiles >>= \datums -> forM_ datums $ writeChan out
              in U.readChan ticker >> writeAction >> clearPercentiles pstate
        in go $ forever $ channelAction
  action `catch` \(_ :: ChannelException) -> putStrLnIO "PercentileBuffer died" >> return ()
  recordAction
  return inC

mkNullRecorder :: ( MonadCatch m
                  , MonadIO m
                  , ChanLike ci co IstatdDatum
                  )
               => m (ci IstatdDatum)
mkNullRecorder = do
  (inC, outC) <- newZChan
  let action = go $ forever $ void $ readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

mkPipeRecorder :: ( MonadCatch m
                  , MonadIO m
                  , ChanLike ci co IstatdDatum
                  )
               => ci IstatdDatum
               -> m (ci IstatdDatum)
mkPipeRecorder c = do
  (inC, outC) <- newZChan
  let action = go $ forever $ writeChan c =<< (readChan outC)
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

mkPrintingRecorder :: ( MonadCatch m
                      , MonadIO m
                      , ChanLike ci co IstatdDatum
                      )
                   => m (ci IstatdDatum)
mkPrintingRecorder = do
  (inC, outC) <- newZChan
  let action = go $ forever $ putStrLn . show =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

mkPrintingEncodedRecorder :: ( MonadCatch m
                             , MonadIO m
                             , ChanLike ci co IstatdDatum
                             )
                          => m (ci IstatdDatum)
mkPrintingEncodedRecorder = do
  (inC, outC) <- newZChan
  let action = go $ forever $ putStrLn . show . toPacket =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

mkIstatdRecorder :: ( MonadCatch m
                    , MonadIO m
                    , ChanLike ci co IstatdDatum
                    )
                 => IstatdConfig
                 -> m (ci IstatdDatum)
mkIstatdRecorder config = do
  (inC, outC) <- newZChan
  connection <- connect config
  let send' p = send connection [p]
  let action = go $ forever $ send' =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC



putStrLnIO :: (MonadIO m)
           => String
           -> m ()
putStrLnIO = liftIO . putStrLn

-- @treed
go :: MonadIO m
   => IO a
   -> m ()
go = liftIO . void . async
