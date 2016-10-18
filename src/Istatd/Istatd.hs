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
, SupportsTime (..)
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
, State (..)
, runAppM
, runAppM'
)
where

import            Control.Concurrent.Async.Lifted     ( async )
import            Control.Exception.Safe              ( catch )
import            Control.Monad                       ( void
                                                      , forever
                                                      , forM_
                                                      , (=<<)
                                                      , (>=>)
                                                      , (<=<)
                                                      )
import            Control.Monad.Catch                 ( MonadCatch )
import            Control.Monad.IO.Class              ( MonadIO
                                                      , liftIO
                                                      )
import            Control.Monad.Trans.Control         ( MonadBaseControl )
import            Data.Monoid                         ( (<>) )
import            Istatd.Chan.ChanLike                ( ChanLike (..)
                                                      , ChannelException (..)
                                                      )
import            Istatd.Client                       ( IstatdConfig (..)
                                                      , connect
                                                      , send
                                                      )
import            Istatd.Control.Monad                ( (<<) )
import            Istatd.Datum.DifferenceCounter      ( DifferenceCounter (..)
                                                      , DifferenceState (..)
                                                      , mkDifferenceState
                                                      , computeDifferenceCounter
                                                      )
import            Istatd.Datum.Percentile             ( Percentile
                                                      , mkPercentile
                                                      , mkPercentiles
                                                      , mkPercentileState
                                                      , clearPercentiles
                                                      , computePercentiles
                                                      , addPercentile
                                                      )
import            Istatd.Monad.Types                  ( runAppM
                                                      , runAppM'
                                                      , State (..)
                                                      )
import            Istatd.Class.Time                   ( SupportsTime (..) )
import            Istatd.Types                        ( IstatdDatum(..)
                                                      , IstatdType (..)
                                                      , FilterFunc
                                                      , FilterFuncT
                                                      , updateKey
                                                      , getKey
                                                      , toPacket
                                                      )

import qualified  Control.Concurrent.Chan.Unagi       as U
import qualified  Data.ByteString.Lazy.Builder        as BSLB



-- | Composes a sink, a specific (possibly differently typed) source, and many
-- filters of the sink type into a new sink.
mkFilterPipelineWithSource
  :: (MonadIO m)
  => ci IstatdDatum
  -> FilterFuncT (ci IstatdDatum) (ci' a) m
  -> [FilterFunc (ci IstatdDatum) m]
  -> m (ci' a)
mkFilterPipelineWithSource sink source fs =
  source <=< foldr (>=>) return fs $ sink

-- | Composes a sink and many filters of the sink type into a new sink.
mkFilterPipeline
  :: (MonadIO m)
  => ci IstatdDatum
  -> [FilterFunc (ci IstatdDatum) m]
  -> m (ci IstatdDatum)
mkFilterPipeline sink fs =
  foldr (>=>) return fs $ sink

-- | Difference filter. Allows the recording of monotonically increasing counters as
-- the rate at which they change. Needs to come first in a pipeline, see `mkFilterPipelineWithSource`.
mkFilterDifference
  :: ( MonadIO m
     , ChanLike ci co IstatdDatum
     , ChanLike ci' co' DifferenceCounter
     )
  => DifferenceState
  -> FilterFuncT (ci IstatdDatum) (ci' DifferenceCounter) m
mkFilterDifference dstate = \out -> do
  (inC, outC) <- newZChan
  let action dc = writeChan out =<< computeDifferenceCounter dstate dc
  go_ $ forever $ action =<< readChan outC
  return inC

-- | Adds a prefix to the key of any `IstatdDatum` that passes through the pipeline
mkFilterPrefix
  :: ( MonadIO m
     , ChanLike ci co IstatdDatum
     )
  => BSLB.Builder
  -> FilterFunc (ci IstatdDatum) m
mkFilterPrefix prefix = \out -> do
  (inC, outC) <- newZChan
  let action r@(getKey -> k) =
        let nk = prefix <> k
        in writeChan out $ updateKey nk r
  go_ $ forever $ action =<< readChan outC
  return inC

-- | Adds a suffix to the key of any `IstatdDatum` that passes through the pipeline
mkFilterSuffix
  :: ( MonadIO m
     , ChanLike ci co IstatdDatum
     )
  => BSLB.Builder
  -> FilterFunc (ci IstatdDatum) m
mkFilterSuffix suffix = \out -> do
  (inC, outC) <- newZChan
  let action r@(getKey -> k) =
        let nk = k <> suffix
        in writeChan out $ updateKey nk r
  go_ $ forever $ action =<< readChan outC
  return inC

-- | Creates a sized buffer for messages going through this pipeline
mkBuffer
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co IstatdDatum
     )
  => Int
  -> FilterFunc (ci IstatdDatum) m
mkBuffer size = \out -> do
  (inC, outC) <- newBChan size
  let action = go_ $ forever $ writeChan out =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Buffer died" >> return ()
  return inC

-- | Creates a sized buffer for messages going through this pipeline ala `mkBuffer`. Additionally
-- records its own length every 1 second to the pipeline.
mkMonitoredBuffer
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co IstatdDatum
     , SupportsTime m
     , MonadBaseControl IO m
     )
  => BSLB.Builder
  -> Int
  -> FilterFunc (ci IstatdDatum) m
mkMonitoredBuffer name size = \out -> do
  (inC, outC) <- newBChan size
  let action = go_ $ forever $ writeChan out =<< (readChan outC)
  ticker <- tick 1
  let recordAction =
        let channelAction =
              let datum len t = IstatdDatum Gauge name t (fromIntegral len)
                  writeAction len = writeChan out . datum len =<< getPOSIXTime
              in (liftIO $ U.readChan ticker) >> (writeAction =<< inChanLen inC)
        in goM_ $ forever $ channelAction
  action `catch` \(_ :: ChannelException) -> putStrLnIO "MonitoredBuffer died" >> return ()
  recordAction
  return inC

-- | Creates a sized buffer for messages going through this pipeline ala `mkBuffer`. Additionally
-- applies a microsecond delay to the messages
mkSlowdownBuffer
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co IstatdDatum
     , SupportsTime m
     , MonadBaseControl IO m
     )
  => Int
  -> Int
  -> FilterFunc (ci IstatdDatum) m
mkSlowdownBuffer time size = \out -> do
  (inC, outC) <- newBChan size
  let action = goM_ $ forever $ writeChan out =<< (readChan outC << threadDelay time)
  action `catch` \(_ :: ChannelException) -> putStrLnIO "MonitoredBuffer died" >> return ()
  return inC

-- | Internally accumulates and computes percentile data for gauges that pass through this pipline.
-- The internal accumulation will be reported and flushed every n seconds.
-- The original gauge will have .raw appended to its name, and a number of other gauages
-- will be reported using the format 'key.{percentile}th'
mkPercentileFilter
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co IstatdDatum
     , SupportsTime m
     , MonadBaseControl IO m
     )
  => Int
  -> [Percentile]
  -> FilterFunc (ci IstatdDatum) m
mkPercentileFilter seconds percentiles = \out -> do
  (inC, outC) <- newZChan
  pstate <- mkPercentileState
  let collectPercentile d@(IstatdDatum Gauge k _ v) = do
        addPercentile pstate (BSLB.toLazyByteString k) v
        return $ updateKey (k <> BSLB.lazyByteString ".raw") d
      collectPercentile d = return d
      action = go_ $ forever $ writeChan out =<< collectPercentile =<< (readChan outC)
  ticker <- tick seconds
  let recordAction =
        let channelAction =
              let writeAction = computePercentiles pstate percentiles >>= \datums -> forM_ datums $ writeChan out
              in (liftIO $ U.readChan ticker) >> writeAction >> clearPercentiles pstate
        in goM_ $ forever $ channelAction
  action `catch` \(_ :: ChannelException) -> putStrLnIO "PercentileBuffer died" >> return ()
  recordAction
  return inC

-- | Simple sink, Reads the pipeline into void
mkNullRecorder
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co IstatdDatum
     )
  => m (ci IstatdDatum)
mkNullRecorder = do
  (inC, outC) <- newZChan
  let action = go_ $ forever $ void $ readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

-- | A sink that allows a channel to be passed in and handled elsewhere
mkPipeRecorder
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co IstatdDatum
     )
  => ci IstatdDatum
  -> m (ci IstatdDatum)
mkPipeRecorder c = do
  (inC, outC) <- newZChan
  let action = go_ $ forever $ writeChan c =<< (readChan outC)
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

-- | A sink that prints to stdout using show
mkPrintingRecorder
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co IstatdDatum
     )
  => m (ci IstatdDatum)
mkPrintingRecorder = do
  (inC, outC) <- newZChan
  let action = go_ $ forever $ putStrLn . show =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

-- | A sink that prints to stdout using `toPacket`
mkPrintingEncodedRecorder
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co IstatdDatum
     )
  => m (ci IstatdDatum)
mkPrintingEncodedRecorder = do
  (inC, outC) <- newZChan
  let action = go_ $ forever $ putStrLn . show . toPacket =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

-- | A sink the record to an istatd agent
mkIstatdRecorder
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co IstatdDatum
     )
  => IstatdConfig
  -> m (ci IstatdDatum)
mkIstatdRecorder config = do
  (inC, outC) <- newZChan
  connection <- connect config
  let send' p = send connection [p]
  let action = go_ $ forever $ send' =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC



putStrLnIO
  :: (MonadIO m)
  => String
  -> m ()
putStrLnIO =
  liftIO . putStrLn

-- @treed
go_
  :: MonadIO m
  => IO a
  -> m ()
go_ =
  liftIO . void . async

goM_
  :: MonadBaseControl IO m
  => m a
  -> m ()
goM_ =
  void . async
