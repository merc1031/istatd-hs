{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Istatd.Istatd
( IstatInChan
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
, iWriteChan
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
                                                  , IstatInChan
                                                  , FilterFunc
                                                  , updateKey
                                                  , getKey
                                                  , toPacket
                                                  )
import            Istatd.Tick                     ( tick )
import            Istatd.Chan                     ( newZChan
                                                  , newBChan
                                                  , iWriteChan
                                                  , iReadChan
                                                  , iInChanLen
                                                  )
import            Istatd.Control.Monad            ( (<<) )

import qualified  Control.Concurrent.Chan.Unagi   as U
import qualified  Data.ByteString.Lazy.Builder    as BSLB
import qualified  Data.Time.Clock.POSIX           as POSIX



mkFilterPipeline :: (MonadIO m)
                 => IstatInChan
                 -> [FilterFunc m]
                 -> m IstatInChan
mkFilterPipeline sink fs = foldr (>=>) return fs $ sink

mkFilterPrefix :: (MonadIO m)
               => BSLB.Builder
               -> FilterFunc m
mkFilterPrefix prefix = \out -> do
    (inC, outC) <- newZChan
    let action r@(getKey -> k) = let nk = prefix <> k
                                 in iWriteChan out $ updateKey nk r
    liftIO $ void $ async $ forever $ action =<< iReadChan outC
    return inC

mkFilterSuffix :: (MonadIO m)
               => BSLB.Builder
               -> FilterFunc m
mkFilterSuffix suffix = \out -> do
    (inC, outC) <- newZChan
    let action r@(getKey -> k) = let nk = k <> suffix
                                 in iWriteChan out $ updateKey nk r
    liftIO $ void $ async $ forever $ action =<< iReadChan outC
    return inC

mkBuffer :: ( MonadCatch m
            , MonadIO m
            )
         => Int
         -> FilterFunc m
mkBuffer size = \out -> do
    (inC, outC) <- newBChan size
    let action = liftIO $ void $ async $ forever $ iWriteChan out =<< iReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Buffer died" >> return ()
    return inC

mkMonitoredBuffer :: ( MonadCatch m
                     , MonadIO m
                     )
                  => BSLB.Builder
                  -> Int
                  -> FilterFunc m
mkMonitoredBuffer name size = \out -> do
    (inC, outC) <- newBChan size
    let action = liftIO $ void $ async $ forever $ iWriteChan out =<< (iReadChan outC)
    ticker <- tick 1
    let recordAction =
            let channelAction =
                    let datum len t = IstatdDatum Gauge name t (fromIntegral len)
                        writeAction len = iWriteChan out . datum len =<< POSIX.getPOSIXTime
                    in U.readChan ticker >> (writeAction =<< iInChanLen inC)
            in liftIO $ void $ async $ forever $ channelAction
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "MonitoredBuffer died" >> return ()
    recordAction
    return inC

mkSlowdownBuffer :: ( MonadCatch m
                    , MonadIO m
                    )
                 => Int
                 -> Int
                 -> FilterFunc m
mkSlowdownBuffer time size = \out -> do
    (inC, outC) <- newBChan size
    let action = liftIO $ void $ async $ forever $ iWriteChan out =<< (iReadChan outC << threadDelay time)
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "MonitoredBuffer died" >> return ()
    return inC

mkNullRecorder :: ( MonadCatch m
                  , MonadIO m
                  )
               => m IstatInChan
mkNullRecorder = do
    (inC, outC) <- newZChan
    let action = liftIO $ void $ async $ forever $ void $ iReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Recorder died" >> return ()
    return inC

mkPrintingRecorder :: ( MonadCatch m
                      , MonadIO m
                      )
                   => m IstatInChan
mkPrintingRecorder = do
    (inC, outC) <- newZChan
    let action = liftIO $ void $ async $ forever $ putStrLn . show =<< iReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Recorder died" >> return ()
    return inC

mkPrintingEncodedRecorder :: ( MonadCatch m
                             , MonadIO m
                             )
                          => m IstatInChan
mkPrintingEncodedRecorder = do
    (inC, outC) <- newZChan
    let action = liftIO $ void $ async $ forever $ putStrLn . show . toPacket =<< iReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Recorder died" >> return ()
    return inC

mkIstatdRecorder :: ( MonadCatch m
                    , MonadIO m
                    )
                 => IstatdConfig
                 -> m IstatInChan
mkIstatdRecorder config = do
    (inC, outC) <- newZChan
    connection <- connect config
    let send' p = send connection [p]
    let action = liftIO $ void $ async $ forever $ send' =<< iReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Recorder died" >> return ()
    return inC



putStrLnIO :: (MonadIO m)
           => String -> m ()
putStrLnIO = liftIO . putStrLn


