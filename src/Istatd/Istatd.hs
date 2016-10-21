{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-} --- For filter hAskey
module Istatd.Istatd
( ChanLike (..)
, IstatdDatum (..)
, IstatdType (..)
, DifferenceCounter (..)
, ChannelException (..)
, FilterFunc
, FilterFuncT
, SupportsTime (..)
, prependF
, (.<:.)
, appendF
, (.:>.)
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

--import Data.Maybe

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
import            Data.Proxy
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
                                                      , HasKey (..)
                                                      , FilterFunc
                                                      , FilterFuncT
                                                      , toPacket
                                                      )

import            Unsafe.Coerce
import            Istatd.TypeSet
import            IfCxt

import qualified  Control.Concurrent.Chan.Unagi       as U
import qualified  Data.ByteString.Lazy.Builder        as BSLB

infixl 1 .:>., `appendF`
infixr 1 .<:., `prependF`

mkPipelineWithSinkM
  :: ( MonadIO m
     )
  => m (ci (Variant '[IstatdDatum]))
  -> FilterFuncT (ci (Variant '[IstatdDatum])) (ci a) m
  -> m (ci a)
mkPipelineWithSinkM sinkA pipe = do
  sink <- sinkA
  mkPipelineWithSink sink pipe

mkPipelineWithSink
  :: ( MonadIO m
     )
  => ci (Variant '[IstatdDatum])
  -> FilterFuncT (ci (Variant '[IstatdDatum])) (ci a) m
  -> m (ci a)
mkPipelineWithSink sink pipe = do
  inC <- pipe sink
  return inC


-- | Prepend a filter in the pipeline.
-- Synonym for `.<:.`, analagous to `<=<`
prependF
  :: forall (m :: * -> *) (ci :: * -> *) injest interA interB next
   . ( Monad m
     , 'True ~ VariantCompatibility (Variant interA) (Variant interB)
     )
  => FilterFuncT (ci (Variant interA)) (ci (Variant injest)) m
  -- ^ Rest of the pipeline
  -> FilterFuncT (ci (Variant next)) (ci (Variant interB)) m
  -- ^ Sink end to add to the pipeline chain
  -> FilterFuncT (ci (Variant next)) (ci (Variant injest)) m
  -- ^ New sink end of the current pipeline chain
prependF = (.<:.)

-- | Append a filter in the pipeline.
-- Synonym for `.:>.`, analagous to `>=>`
appendF
  :: forall (m :: * -> *) (ci :: * -> *) injest interA interB next
   . ( Monad m
     , 'True ~ VariantCompatibility (Variant interA) (Variant interB)
     )
  => FilterFuncT (ci (Variant interA)) (ci (Variant injest)) m
  -- ^ Rest of the pipeline
  -> FilterFuncT (ci (Variant next)) (ci (Variant interB)) m
  -- ^ Sink end to add to the pipeline chain
  -> FilterFuncT (ci (Variant next)) (ci (Variant injest)) m
  -- ^ New sink end of the current pipeline chain
appendF = (.:>.)

-- | Prepend a filter in the pipeline.
-- Synonym for `.<:.`, analagous to `<=<`
(.<:.)
  :: forall (m :: * -> *) (ci :: * -> *) injest interA interB next
   . ( Monad m
     , 'True ~ VariantCompatibility (Variant interA) (Variant interB)
     )
  => FilterFuncT (ci (Variant interA)) (ci (Variant injest)) m
  -- ^ Rest of the pipeline
  -> FilterFuncT (ci (Variant next)) (ci (Variant interB)) m
  -- ^ Sink end to add to the pipeline chain
  -> FilterFuncT (ci (Variant next)) (ci (Variant injest)) m
  -- ^ New sink end of the current pipeline chain
fl .<:. fr = fl <=< fr

-- | Append a filter in the pipeline.
-- Synonym for `.:>.`, analagous to `>=>`
(.:>.)
  :: forall (m :: * -> *) (ci :: * -> *) injest interA interB next
   . ( Monad m
     , 'True ~ VariantCompatibility (Variant interA) (Variant interB)
     )
  => FilterFuncT (ci (Variant interA)) (ci (Variant injest)) m
  -- ^ Rest of the pipeline
  -> FilterFuncT (ci (Variant next)) (ci (Variant interB)) m
  -- ^ Sink end to add to the pipeline chain
  -> FilterFuncT (ci (Variant next)) (ci (Variant injest)) m
  -- ^ New sink end of the current pipeline chain
fl .:>. fr = \x -> (fr x >>= \y -> fl y)

-- | How should these pipelines work
--
--  append a filter lets say we need to sink
-- IstatdDatum    -------- Variant '[IstatdDatum]
--  we want to add a buffer
--  Buffers SHOULD have type   Variant ns  -> Variant ns
-- then we want to add a monitored buffer
--  monitored buffers should have type   Varian ns -> Variant ( AtLeast IstatdDatum ns )
--     this should imply that '[IstatdDatum] is valid,
--     so its             '[Moreshit,IStatdDatum]
--     and so is          '[IstatdDatum,Moreshit]
-- then we want to add a Prefix filter
-- prefix filter should have type  HasKey a =>     Variant (AtLeast a ns ) -> Variant (AtLeast a ns)


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
  :: forall m ci co next next'
   . ( MonadIO m
     , ChanLike ci co (Variant next)
     , ChanLike ci co next'
     , next' ~ (MinVariant (DifferenceCounter ': next))
----     , setnext ~ SetFromVariant next
--     , InVariant injest IstatdDatum ~ 'True
--     , InVariant next IstatdDatum ~ 'True
--     , InVariant injest DifferenceCounter ~ 'True
----     , injest ~ SetToVariant (SetInsert setnext DifferenceCounter)
--     , 'True ~ VariantCompatibility injest next
--     , me ~ VariantDiff injest next
--     , next ~ VariantDiff injest me
     --, Variant nexts ~ next
     --, Variant injests ~ injest
--     , next ~ (UniqueInsert next IstatdDatum)
     , In next IstatdDatum next
--     , injest ~ (UniqueInsert next DifferenceCounter)
--     , In next DifferenceCounter injest
     , SNatRep (FirstIdxList next IstatdDatum)
--     , IElem IstatdDatum next
--     , IElem IstatdDatum injest
--     , ISubset next injest
--     , IElem DifferenceCounter injest
     )
  => DifferenceState
  -> FilterFuncT (ci (Variant next)) (ci next') m
mkFilterDifference dstate = \out -> do
  (inC, outC) <- newZChan
--  let action dc = writeChan out =<< (((fromJust . gconstr) <$> computeDifferenceCounter dstate dc) :: injest)
  let action :: Either DifferenceCounter (Variant next) -> IO ()
      action (Left dc) = do
        ida <- computeDifferenceCounter dstate dc
        writeChan out $ mkVariantV (Proxy :: Proxy (Variant next)) ida
      action  (Right rs) = writeChan out $ unsafeCoerce rs

  go_ $ forever $ (action . splitVariant ) =<< readChan outC
  return inC

-- | Adds a prefix to the key of any `IstatdDatum` that passes through the pipeline
mkFilterPrefix
  :: forall m ci co a next next'
   . ( MonadIO m
     , ChanLike ci co (Variant next)
     , ChanLike ci co next'
     , next' ~ (MinVariant (next))
     )
  => BSLB.Builder
  -> FilterFuncT (ci (Variant next)) (ci next') m
mkFilterPrefix prefix = \out -> do
  (inC, outC) <- newZChan
--  let action (Left r@(getKey -> k)) =
--        let nk = prefix <> k
--        in writeChan out $ mkVariantV (Proxy :: Proxy (Variant next)) $ updateKey r nk
--      action (Right d) = writeChan out $ unsafeCoerce d
--  go_ $ forever $ (action . splitVariant) =<< readChan outC
  let action v = writeChan out v
  go_ $ forever $ (doIt action prefix ifHasKey (\p k -> p <> k)) =<< readChan outC
  return inC

doIt :: (MonadIO m)
     => (Variant hs' -> m ())
     -> BSLB.Builder
     -> (forall a. IfCxt (HasKey a) => (HasKey a => a -> a) -> (a -> a) -> a -> a)
     -> (BSLB.Builder -> BSLB.Builder -> BSLB.Builder)
     -> Variant hs
     -> m ()
doIt a prefix f c v =
  let v' = splitVariant v
  in a $ case v' of
       Left v'' -> mkVariant (Proxy :: Proxy (Variant hs')) $ f (lefter) (id) v''
       Right v'' -> unsafeCoerce $ id v''
    where
      lefter r@(getKey -> k) =
        let nk = c prefix k
        in updateKey r nk


ifHasKey :: forall a. IfCxt (HasKey a) => (HasKey a => a -> a) -> (a -> a) -> a -> a
ifHasKey l r = ifCxt (Proxy :: Proxy (HasKey a)) l r

-- | Adds a suffix to the key of any `IstatdDatum` that passes through the pipeline
mkFilterSuffix
  :: forall m ci co a next next'
   . ( MonadIO m
     , ChanLike ci co next'
     , ChanLike ci co (Variant next)
     , next' ~ (MinVariant (next))
     )
  => BSLB.Builder
  -> FilterFuncT (ci (Variant next)) (ci next') m
mkFilterSuffix suffix = \out -> do
  (inC, outC) <- newZChan
--  let action (Left (r@(getKey -> k))) =
--        let nk = k <> suffix
--        in writeChan out $ mkVariantV (Proxy :: Proxy (Variant next)) $ updateKey r nk
--      action (Right d) = writeChan out $ unsafeCoerce d
--  go_ $ forever $ (action . splitVariant) =<< readChan outC
  let action v = writeChan out v
  go_ $ forever $ (doIt action suffix ifHasKey (\s k -> k <> s)) =<< readChan outC
  return inC

-- | Creates a sized buffer for messages going through this pipeline
mkBuffer
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co a
     )
  => Int
  -> FilterFuncT (ci a) (ci a) m
mkBuffer size = \out -> do
  (inC, outC) <- newBChan size
  let action = go_ $ forever $ writeChan out =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Buffer died" >> return ()
  return inC

-- | Creates a sized buffer for messages going through this pipeline ala `mkBuffer`. Additionally
-- records its own length every 1 second to the pipeline.
mkMonitoredBuffer
  :: forall m ci co next next'
   . ( MonadCatch m
     , MonadIO m
     , ChanLike ci co next'
     , ChanLike ci co (Variant next)
     , SupportsTime m
     , MonadBaseControl IO m
     , next' ~ (MinVariant (IstatdDatum ': next))
     )
  => BSLB.Builder
  -> Int
  -> FilterFuncT (ci (MinVariant (IstatdDatum ': next))) (ci (Variant next)) m
mkMonitoredBuffer name size = \out -> do
  (inC, outC) <- newBChan size
  let action = go_ $ forever $ writeChan out =<< (return . unsafeCoerce) =<< (readChan outC)
  ticker <- tick 1
  let recordAction =
        let channelAction =
              let datum len t = mkVariantV (Proxy :: Proxy (MinVariant (IstatdDatum ': next))) $ IstatdDatum Gauge name t (fromIntegral len)
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
  :: forall m ci co next next'
   . ( MonadCatch m
     , MonadIO m
     , ChanLike ci co (Variant next)
  --   , ChanLike ci co next'
     , ChanLike ci co next'
     , next' ~ (MinVariant (IstatdDatum ': next))
     , Variant next ~ next'
     , 'True ~ InList next IstatdDatum
     , SupportsTime m
     , MonadBaseControl IO m
     )
  => Int
  -> [Percentile]
  -> FilterFuncT (ci next') (ci next') m
mkPercentileFilter seconds percentiles = \out -> do
  (inC, outC) <- newZChan
  pstate <- mkPercentileState
  let collectPercentile (Left d@(IstatdDatum Gauge k _ v)) = do
        addPercentile pstate (BSLB.toLazyByteString k) v
        return $ mkVariantV (Proxy :: Proxy (next')) $ updateKey d (k <> BSLB.lazyByteString ".raw")
      collectPercentile (Left d) = return $ mkVariantV (Proxy :: Proxy (next')) d
      collectPercentile (Right d) = return $ unsafeCoerce d
      action = go_ $ forever $ writeChan out =<< (collectPercentile . splitVariant) =<< (readChan outC)
  ticker <- tick seconds
  let recordAction =
        let channelAction =
              let writeAction = computePercentiles pstate percentiles >>= \datums -> forM_ datums $ \d -> writeChan out $ mkVariantV (Proxy :: Proxy (next')) d
              in (liftIO $ U.readChan ticker) >> writeAction >> clearPercentiles pstate
        in goM_ $ forever $ channelAction
  action `catch` \(_ :: ChannelException) -> putStrLnIO "PercentileBuffer died" >> return ()
  recordAction
  return inC

-- | Simple sink, Reads the pipeline into void
mkNullRecorder
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co (MinVariant '[IstatdDatum])
     )
  => m (ci (MinVariant '[IstatdDatum]))
mkNullRecorder = do
  (inC, outC) <- newZChan
  let action = go_ $ forever $ void $ readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

-- | A sink that allows a channel to be passed in and handled elsewhere
mkPipeRecorder
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co (MinVariant '[IstatdDatum])
     )
  => ci (MinVariant '[IstatdDatum])
  -> m (ci (MinVariant '[IstatdDatum]))
mkPipeRecorder c = do
  (inC, outC) <- newZChan
  let action = go_ $ forever $ writeChan c =<< (readChan outC)
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

-- | A sink that prints to stdout using show
mkPrintingRecorder
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co (MinVariant '[IstatdDatum])
     )
  => m (ci (MinVariant '[IstatdDatum]))
mkPrintingRecorder = do
  (inC, outC) <- newZChan
  let action = go_ $ forever $ putStrLn . show =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

-- | A sink that prints to stdout using `toPacket`
mkPrintingEncodedRecorder
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co (MinVariant '[IstatdDatum])
     )
  => m (ci (MinVariant '[IstatdDatum]))
mkPrintingEncodedRecorder = do
  (inC, outC) <- newZChan
  let action = go_ $ forever $ putStrLn . show . toPacket . (\(Left d) -> d) . splitVariant =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

-- | A sink the record to an istatd agent
mkIstatdRecorder
  :: ( MonadCatch m
     , MonadIO m
     , ChanLike ci co (MinVariant '[IstatdDatum])
     )
  => IstatdConfig
  -> m (ci (MinVariant '[IstatdDatum]))
mkIstatdRecorder config = do
  (inC, outC) <- newZChan
  connection <- connect config
  let send' (Left p) = send connection [p]
      send' (Right _) = return () -- absurd?
  let action = go_ $ forever $ (send' . splitVariant) =<< readChan outC
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
