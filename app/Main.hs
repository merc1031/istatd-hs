{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import            Data.Proxy
import            Control.Monad           ( forever )
import            Control.Monad.IO.Class  ( liftIO )
import            Istatd.Istatd
import            Istatd.TypeSet
import            System.Random

import qualified  Istatd.Chan.ChanT       as ChanT

main :: IO ()
main = runAppM' $ do
  (sink :: ChanT.InChanI (Variant '[IstatdDatum])) <- mkPrintingEncodedRecorder
  stats <- mkPipelineWithSink sink .:>. mkMonitoredBuffer "monitor" 5
                                   .:>. mkFilterPrefix "Hi."
                                   .:>. mkFilterSuffix ".no"


  let percentiles = mkPercentiles [10, 90, 95, 99]

  percetilestats <- mkPipelineWithSink sink .:>. mkPercentileFilter 5 percentiles
                                            .:>. mkFilterPrefix "Another."
                                            .:>. mkFilterSuffix ".Pipeline"

  forever $ do
    t <- getPOSIXTime
    writeChan stats $ mkVariantV (Proxy :: Proxy (Variant '[IstatdDatum])) (IstatdDatum Counter "name" t 100)

    r <- liftIO $ getStdRandom (randomR (1,6 :: Double))

    writeChan percetilestats $ mkVariantV (Proxy :: Proxy (Variant '[IstatdDatum])) (IstatdDatum Gauge "gaugy" t r)
    writeChan percetilestats $ mkVariantV (Proxy :: Proxy (Variant '[IstatdDatum])) (IstatdDatum Counter "countery" t r)

    threadDelay 100000
