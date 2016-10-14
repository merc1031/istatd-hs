{-# LANGUAGE OverloadedStrings #-}
module Main where

import            Control.Concurrent      ( threadDelay )
import            Control.Monad           ( forever )
import            Istatd.Istatd
import qualified  Data.Time.Clock.POSIX   as POSIX

main :: IO ()
main = do
  sink <- mkPrintingEncodedRecorder
  stats <- mkFilterPipeline sink [mkMonitoredBuffer "monitor" 5, mkFilterPrefix "Hi.", mkFilterSuffix ".no"]

  forever $ do
    t <- POSIX.getPOSIXTime
    iWriteChan stats (IstatdDatum Counter "name" t 100)
    threadDelay 100000
