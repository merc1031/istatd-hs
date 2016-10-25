{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import            Control.Monad           ( forever )
import            Control.Monad.IO.Class  ( liftIO )
import            Istatd.Istatd
import            System.Random

import qualified  Istatd.Chan.ChanT       as ChanT
import Istatd.Simplicity

main :: IO ()
main = runAppM' $ do
  (sink :: ChanT.InChanI (Summed '[IstatdDatum])) <- mkPrintingEncodedRecorder
  stats <- mkPipelineWithSink sink $ mkMonitoredBuffer "monitor" 5
                                .:>. mkFilterPrefix "Hi."
                                .:>. mkFilterSuffix ".no"



  let percentiles = mkPercentiles [10, 90, 95, 99]

  percetilestats <- mkPipelineWithSink sink $ mkPercentileFilter 5 percentiles
                                         .:>. mkFilterPrefix "Another."
                                         .:>. mkFilterSuffix ".Pipeline"

  forever $ do
    t <- getPOSIXTime
    writeChan stats (IstatdDatum Counter "name" t 100)

    r <- liftIO $ getStdRandom (randomR (1,6 :: Double))

    writeChan percetilestats (IstatdDatum Gauge "gaugy" t r)
    writeChan percetilestats (IstatdDatum Counter "countery" t r)

    threadDelay 100000
