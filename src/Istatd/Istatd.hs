{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Istatd.Istatd
( ChanLike (..)
, IstatdDatum (..)
, IstatdType (..)
, DifferenceCounter (..)
, ChannelException (..)
, FilterFunc
, FilterFuncT
, SupportsTime (..)
, (.<:.)
, (.:>.)
, appendF
, prependF
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
, mkPipelineWithSink
, mkPipelineWithSinkM
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
                                                      , DifferenceCounterIsh (..)
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
                                                      , IstatdData (..)
                                                      , IstatdType (..)
                                                      , HasKey (..)
                                                      , FilterFunc
                                                      , FilterFuncT
                                                      , toPacket
                                                      )
import Istatd.Complexity
import            IfCxt
import            Data.Proxy

import qualified  Control.Concurrent.Chan.Unagi       as U
import qualified  Data.ByteString.Lazy.Builder        as BSLB

mkIfCxtInstances ''HasKey

infixl 1 .:>., `appendF`
infixr 1 .<:., `prependF`

mkPipelineWithSinkM
  :: ( MonadIO m
     )
  => m (ci IstatdDatum)
  -> FilterFuncT (ci IstatdDatum) (ci a) m
  -> m (ci a)
mkPipelineWithSinkM sinkA pipe = do
  sink <- sinkA
  mkPipelineWithSink sink pipe

mkPipelineWithSink
  :: ( MonadIO m
     )
  => ci IstatdDatum
  -> FilterFuncT (ci IstatdDatum) (ci a) m
  -> m (ci a)
mkPipelineWithSink sink pipe = do
  inC <- pipe sink
  return inC


-- | Prepend a filter in the pipeline.
-- Synonym for `.<:.`, analagous to `<=<`
prependF
  :: forall (m :: * -> *) (ci :: * -> *) injest inter next
   . ( Monad m
     )
  => FilterFuncT (ci next) (ci inter) m
  -- ^ Rest of the pipeline
  -> FilterFuncT (ci inter) (ci injest) m
  -- ^ Sink end to add to the pipeline chain
  -> FilterFuncT (ci next) (ci injest) m
  -- ^ New sink end of the current pipeline chain
prependF = (.<:.)

-- | Append a filter in the pipeline.
-- Synonym for `.:>.`, analagous to `>=>`
appendF
  :: forall (m :: * -> *) (ci :: * -> *) injest inter next
   . ( Monad m
     )
  => FilterFuncT (ci inter) (ci injest) m
  -- ^ Rest of the pipeline
  -> FilterFuncT (ci next) (ci inter) m
  -- ^ Sink end to add to the pipeline chain
  -> FilterFuncT (ci next) (ci injest) m
  -- ^ New sink end of the current pipeline chain
appendF = (.:>.)

--- | Prepend a filter in the pipeline.
--- Synonym for `.<:.`, analagous to `<=<`
(.<:.)
  :: forall (m :: * -> *) (ci :: * -> *) injest inter next
   . ( Monad m
     )
  => FilterFuncT (ci next) (ci inter) m
  -- ^ Rest of the pipeline
  -> FilterFuncT (ci inter) (ci injest) m
  -- ^ Sink end to add to the pipeline chain
  -> FilterFuncT (ci next) (ci injest) m
  -- ^ New sink end of the current pipeline chain
fl .<:. fr = fl >=> fr

--writeIt :: forall ci c a m (cs :: [* -> *])
--         . ( Sendable a :<: cs
--           )
--        => ci cs -> a -> m ()
--writeIt chan item = writeChan chan $ inj $ mkSendable item

-- | Append a filter in the pipeline.
-- Synonym for `.:>.`, analagous to `>=>`
(.:>.)
  :: forall (m :: * -> *) (ci :: * -> *) injest inter next
   . ( Monad m
     )
  => FilterFuncT (ci inter) (ci injest) m
  -- ^ Rest of the pipeline
  -> FilterFuncT (ci next) (ci inter) m
  -- ^ Sink end to add to the pipeline chain
  -> FilterFuncT (ci next) (ci injest) m
  -- ^ New sink end of the current pipeline chain
fl .:>. fr = fl <=< fr


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
  :: forall m ci co a
   . ( MonadIO m
     , ChanLike ci co IstatdDatum
     , ChanLike ci co a
     , IfCxt (DifferenceCounterIsh a)
     , IfCxt (IstatdData a)
     , MonadBaseControl IO m
     )
  => DifferenceState
  -> FilterFuncT (ci IstatdDatum) (ci a) m
mkFilterDifference dstate = \out -> do
  (inC, outC) <- newZChan
  let --action dc = writeChan out =<< computeDifferenceCounter dstate dc
      ifDiffCounter :: a -> m ()
      ifDiffCounter v = writeChan out =<< (ifCxt (Proxy :: Proxy (DifferenceCounterIsh a))
                                                (computeDifferenceCounter dstate . toDiff)
                                                (\s -> return $ ifCxt (Proxy :: Proxy (IstatdData a))
                                                                      toData
                                                                      (error "Difference needs to resolve to istatddata")
                                                                      s
                                                ) v
                                          )
  goM_ $ forever $ ifDiffCounter =<< readChan outC
  return inC

-- | Adds a prefix to the key of any `IstatdDatum` that passes through the pipeline
mkFilterPrefix
  :: forall m ci co a
   . ( MonadIO m
     , ChanLike ci co a
     , MonadBaseControl IO m
     , IfCxt (HasKey a)
     )
  => BSLB.Builder
  -> FilterFuncT (ci a) (ci a) m
mkFilterPrefix prefix = \out -> do
  (inC, outC) <- newZChan
  let uk :: (HasKey a) => a -> a
      uk r@(getKey -> k) =
        let nk = prefix <> k
        in updateKey r nk
      ifHasKey :: a -> m ()
      ifHasKey v = writeChan out $ ifCxt (Proxy :: Proxy (HasKey a)) uk (id) v
  goM_ $ forever $ ifHasKey =<< readChan outC
  return inC

-- | Adds a suffix to the key of any `IstatdDatum` that passes through the pipeline
mkFilterSuffix
  :: forall m ci co a
   . ( MonadIO m
     , ChanLike ci co a
     , MonadBaseControl IO m
     , IfCxt (HasKey a)
     )
  => BSLB.Builder
  -> FilterFuncT (ci a) (ci a) m
mkFilterSuffix suffix = \out -> do
  (inC, outC) <- newZChan
  let uk :: (HasKey a) => a -> a
      uk r@(getKey -> k) =
        let nk = k <> suffix
        in updateKey r nk
      ifHasKey :: a -> m ()
      ifHasKey v = writeChan out $ ifCxt (Proxy :: Proxy (HasKey a)) uk (id) v
  goM_ $ forever $ ifHasKey =<< readChan outC
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
        return $ updateKey d (k <> BSLB.lazyByteString ".raw")
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
     , ChanLike ci co a
     , ChanLike ci co IstatdDatum
     , IstatdData a
     )
  => ci IstatdDatum
  -> m (ci a)
mkPipeRecorder c = do
  (inC, outC) <- newZChan
  let action = go_ $ forever $ (writeChan c . toData) =<< (readChan outC)
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
