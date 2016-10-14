{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Istatd.Istatd
( IstatInChan
, Rec (..)
, mkFilterPipeline
, mkBuffer
, mkFilterPrefix
, mkFilterSuffix
, mkMonitoredBuffer
, mkSlowdownBuffer
, mkNullRecorder
, mkPrintingRecorder
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
import            Istatd.Types                    ( Rec(..)
                                                  , IstatInChan
                                                  , FilterFunc
                                                  , updateKey
                                                  , getKey
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
import qualified  Data.Text.Lazy                  as TL



mkFilterPipeline :: (MonadIO m)
                 => IstatInChan
                 -> [FilterFunc m]
                 -> m IstatInChan
mkFilterPipeline sink fs = foldr (>=>) return fs $ sink

mkFilterPrefix :: (MonadIO m)
               => TL.Text
               -> FilterFunc m
mkFilterPrefix prefix = \out -> do
    (inC, outC) <- newZChan
    let action r@(getKey -> k) = let nk = TL.append prefix k
                                 in iWriteChan out $ updateKey nk r
    liftIO $ void $ async $ forever $ action =<< iReadChan outC
    return inC

mkFilterSuffix :: (MonadIO m)
               => TL.Text
               -> FilterFunc m
mkFilterSuffix suffix = \out -> do
    (inC, outC) <- newZChan
    let action r@(getKey -> k) = let nk = TL.append k suffix
                                 in iWriteChan out $ updateKey nk r
    liftIO $ void $ async $ forever $ action =<< iReadChan outC
    return inC

mkBuffer :: (MonadCatch m, MonadIO m)
         => Int
         -> FilterFunc m
mkBuffer size = \out -> do
    (inC, outC) <- newBChan size
    let action = liftIO $ void $ async $ forever $ iWriteChan out =<< iReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Buffer died" >> return ()
    return inC

mkMonitoredBuffer :: (MonadCatch m, MonadIO m)
                  => TL.Text
                  -> Int
                  -> FilterFunc m
mkMonitoredBuffer name size = \out -> do
    (inC, outC) <- newBChan size
    let action = liftIO $ void $ async $ forever $ iWriteChan out =<< (iReadChan outC)
    ticker <- tick 1
    let recordAction = liftIO $ void $ async $ forever $ U.readChan ticker >> ((\c'' -> (iWriteChan out $ Gauge name 1 (fromIntegral c''))) =<< iInChanLen inC)
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "MonitoredBuffer died" >> return ()
    recordAction
    return inC

mkSlowdownBuffer :: (MonadCatch m, MonadIO m)
                 => Int
                 -> Int
                 -> FilterFunc m
mkSlowdownBuffer time size = \out -> do
    (inC, outC) <- newBChan size
    let action = liftIO $ void $ async $ forever $ iWriteChan out =<< (iReadChan outC << threadDelay time)
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "MonitoredBuffer died" >> return ()
    return inC

mkNullRecorder :: (MonadCatch m, MonadIO m)
               => m IstatInChan
mkNullRecorder = do
    (inC, outC) <- newZChan
    let action = liftIO $ void $ async $ forever $ void $ iReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Recorder died" >> return ()
    return inC

mkPrintingRecorder :: (MonadCatch m, MonadIO m)
                   => m IstatInChan
mkPrintingRecorder = do
    (inC, outC) <- newZChan
    let action = liftIO $ void $ async $ forever $ putStrLn . show =<< iReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLnIO "Recorder died" >> return ()
    return inC

putStrLnIO :: (MonadIO m)
           => String -> m ()
putStrLnIO = liftIO . putStrLn


