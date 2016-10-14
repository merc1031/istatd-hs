{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Istatd

main :: IO ()
main = do
  sink <- mkPrintingRecorder
  stats <- mkFilterPipeline sink [mkMonitoredBuffer "monitor" 5, mkFilterPrefix "Hi.", mkFilterSuffix ".no"]

  forever $ do
    iWriteChan stats (Counter "name" 1 100)
    threadDelay 100000
