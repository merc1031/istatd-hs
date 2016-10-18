{-# LANGUAGE OverloadedStrings #-}
module Main where

import            Control.Concurrent      ( threadDelay )
import            Control.Monad           ( forever )
import            Istatd.Istatd
import            System.Random

import qualified  Data.Time.Clock.POSIX   as POSIX
import qualified  Istatd.Chan.ChanT       as ChanT

main :: IO ()
main = do
  sink <- mkPrintingEncodedRecorder :: IO (ChanT.InChanI IstatdDatum)
  stats <- mkFilterPipeline sink [ mkMonitoredBuffer "monitor" 5
                                 , mkFilterPrefix "Hi."
                                 , mkFilterSuffix ".no"
                                 ]


  let percentiles = mkPercentiles [10, 90, 95, 99]

  percetilestats <- mkFilterPipeline sink [ mkPercentileFilter 5 percentiles
                                          , mkFilterPrefix "Another."
                                          , mkFilterSuffix ".Pipeline"
                                          ]

  forever $ do
    t <- POSIX.getPOSIXTime
    writeChan stats (IstatdDatum Counter "name" t 100)

    r <- getStdRandom (randomR (1,6 :: Double))

    writeChan percetilestats (IstatdDatum Gauge "gaugy" t r)
    writeChan percetilestats (IstatdDatum Counter "countery" t r)

    threadDelay 100000
