{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Istatd.Istatd
( ChanLike (..)
, IstatdDatum (..)
, ChannelException (..)
, FilterFunc
, mkSinkState
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
, mkPipeRecorder
, mkPipeEncodedRecorder
)
where

import            Control.Concurrent              ( threadDelay )
import            Control.Concurrent.Async        ( async )
import            Control.Exception.Safe          ( catch )
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
                                                  , SinkState
                                                  , FilterFunc
                                                  , updateKey
                                                  , getKey
                                                  , toPacket
                                                  , mkSinkState
                                                  )
import            Istatd.Tick                     ( tick )
import            Istatd.Chan.ChanLike            ( ChanLike (..)
                                                  , ChannelException (..)
                                                  )
import            Istatd.Control.Monad            ( (<<) )

import qualified  Control.Concurrent.Chan.Unagi   as U
import qualified  Data.ByteString                 as BS
import qualified  Data.ByteString.Lazy.Builder    as BSLB
import qualified  Data.Time.Clock.POSIX           as POSIX




mkFilterPipeline :: (MonadIO m)
                 => ci IstatdDatum
                 -> [FilterFunc (ci IstatdDatum) m]
                 -> m (ci IstatdDatum)
mkFilterPipeline sink fs = foldr (>=>) return fs $ sink

mkFilterPrefix :: (MonadIO m, ChanLike ci co IstatdDatum)
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
              let datum len t = Gauge name t (fromIntegral len)
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

--type Sink m ci = SinkState -> m (ci IstatdDatum)
mkNullRecorder :: ( MonadCatch m
                  , MonadIO m
                  , ChanLike ci co IstatdDatum
                  )
               => SinkState
               -> m (ci IstatdDatum) --Sink m ci
               --m (ci IstatdDatum)
mkNullRecorder state = do
  (inC, outC) <- newZChan
  let action = go $ forever $ void $ toPacket state =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

mkPipeRecorder :: ( MonadCatch m
                  , MonadIO m
                  , ChanLike ci co IstatdDatum
                  )
               => ci IstatdDatum
               -> SinkState
               -> m (ci IstatdDatum)
mkPipeRecorder c _state = do
  (inC, outC) <- newZChan
  let action = go $ forever $ writeChan c =<< (readChan outC)
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

mkPipeEncodedRecorder :: ( MonadCatch m
                         , MonadIO m
                         , ChanLike ci co IstatdDatum
                         , ChanLike ci' co' BS.ByteString
                         )
                      => ci' BS.ByteString
                      -> SinkState
                      -> m (ci IstatdDatum)
mkPipeEncodedRecorder c state = do
  (inC, outC) <- newZChan
  let action = go $ forever $ writeChan c =<< toPacket state =<< (readChan outC)
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

mkPrintingRecorder :: ( MonadCatch m
                      , MonadIO m
                      , ChanLike ci co IstatdDatum
                      )
                   => SinkState
                   -> m (ci IstatdDatum)
mkPrintingRecorder _state = do
  (inC, outC) <- newZChan
  let action = go $ forever $ putStrLn . show =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

mkPrintingEncodedRecorder :: ( MonadCatch m
                             , MonadIO m
                             , ChanLike ci co IstatdDatum
                             )
                          => SinkState
                          -> m (ci IstatdDatum)
mkPrintingEncodedRecorder state = do
  (inC, outC) <- newZChan
  let action = go $ forever $ putStrLn . show =<< toPacket state =<< readChan outC
  action `catch` \(_ :: ChannelException) -> putStrLnIO "Recorder died" >> return ()
  return inC

mkIstatdRecorder :: ( MonadCatch m
                    , MonadIO m
                    , ChanLike ci co IstatdDatum
                    )
                 => IstatdConfig
                 -> SinkState
                 -> m (ci IstatdDatum)
mkIstatdRecorder config state = do
  (inC, outC) <- newZChan
  connection <- connect config
  let send' p = send connection [p] (toPacket state)
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
