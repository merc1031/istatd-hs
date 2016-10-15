{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Istatd.IstatdSpec where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadCatch)
import Test.Hspec
import Istatd.Istatd
import qualified Istatd.Chan.ChanT as ChanT
import qualified Istatd.Chan.Chan as Chan

mkPipeEnv :: ( MonadIO m
             , MonadCatch m
             )
          => [FilterFunc (ChanT.InChanI IstatdDatum) m]
          -> m (ChanT.OutChanI IstatdDatum, ChanT.InChanI IstatdDatum)
mkPipeEnv ps = do
  (inC, outC) <- newZChan
  sink <- mkPipeRecorder inC
  fp <- mkFilterPipeline sink ps
  return (outC, fp)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "filterPrefix" $ do
    it "adds a prefix to the counter" $ do
        (tchan, pipeline) <- mkPipeEnv [mkFilterPrefix "a"]
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "aCounterName" 0 1)
    it "adds prefixes to the counter in order" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterPrefix "c"
                                       , mkFilterPrefix "b"
                                       , mkFilterPrefix "a"
                                       ]
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "cbaCounterName" 0 1)
    it "adds suffix to the counter" $ do
        (tchan, pipeline) <- mkPipeEnv [mkFilterSuffix "a"]
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "CounterNamea" 0 1)
    it "adds suffixes to the counter in order" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterSuffix "c"
                                       , mkFilterSuffix "b"
                                       , mkFilterSuffix "a"
                                       ]
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "CounterNameabc" 0 1)
    it "adds prefix and suffix to the counter" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterSuffix "b"
                                       , mkFilterPrefix "a"
                                       ]
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "aCounterNameb" 0 1)
    it "adds suffix and prefix to the counter" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterPrefix "b"
                                       , mkFilterSuffix "a"
                                       ]
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "bCounterNamea" 0 1)
    it "adds suffixes and prefixes to the counter in order" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterPrefix "f"
                                       , mkFilterSuffix "e"
                                       , mkFilterSuffix "d"
                                       , mkFilterPrefix "c"
                                       , mkFilterPrefix "b"
                                       , mkFilterSuffix "a"
                                       ]
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "fcbCounterNameade" 0 1)
    it "sends all messages through channel" $ do
        (tchan, pipeline) <- mkPipeEnv []
        replicateM_ 100 $ writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        replicateM_ 100 $ do
            res <- readChan tchan
            res `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
    it "sends all messages through channel buffering" $ do
        (tchan, pipeline) <- mkPipeEnv [mkBuffer 1000]
        replicateM_ 100 $ writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        replicateM_ 100 $ do
            res <- readChan tchan
            res `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
    it "sends all messages through channel buffering" $ do
        (tchan, pipeline) <- mkPipeEnv [mkBuffer 1000]
        replicateM_ 10000 $ writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        replicateM_ 10000 $ do
            res <- readChan tchan
            res `shouldBe` (IstatdDatum Counter "CounterName" 0 1)

