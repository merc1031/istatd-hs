{-# LANGUAGE OverloadedStrings #-}
module Main where

import            Control.Concurrent      ( threadDelay )
import            Control.Monad           ( forever )
import            Istatd.Istatd
import qualified  Data.Time.Clock.POSIX   as POSIX
import qualified  Istatd.Chan.ChanT       as ChanT

main :: IO ()
main = do
  state <- mkSinkState
  sink <- mkPrintingEncodedRecorder state :: IO (ChanT.InChanI IstatdDatum)
  stats <- mkFilterPipeline sink [mkMonitoredBuffer "monitor" 5
                                 , mkFilterPrefix "Hi."
                                 , mkFilterSuffix ".no"
                                 ]

  forever $ do
    t <- POSIX.getPOSIXTime
    writeChan stats (Counter "name" t 100)
    threadDelay 100000
