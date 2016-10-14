{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent           (threadDelay)
import Control.Monad                (forever)
import Istatd.Istatd

main :: IO ()
main = do
  sink <- mkPrintingRecorder
  stats <- mkFilterPipeline sink [mkSlowdownBuffer 1000000 5, mkMonitoredBuffer "monitor" 5, mkFilterPrefix "Hi.", mkFilterSuffix ".no"]

  forever $ do
    iWriteChan stats (Counter "name" 1 100)
    threadDelay 100000
