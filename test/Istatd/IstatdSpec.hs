{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Istatd.IstatdSpec where

import            Control.Exception                 ( catch )
import            Control.Monad                     ( replicateM_
                                                    , forM_
                                                    )
import            Control.Monad.Catch               ( MonadCatch )
import            Control.Monad.IO.Class            ( MonadIO )
import            Istatd.Istatd
import            Test.Hspec
import qualified  Istatd.Chan.Chan                  as Chan
import qualified  Istatd.Chan.ChanT                 as ChanT

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

mkPipeEnvWithSource :: ( MonadIO m
                       , MonadCatch m
                       )
                    => FilterFuncT (ChanT.InChanI IstatdDatum) (ChanT.InChanI a) m
                    -> [FilterFunc (ChanT.InChanI IstatdDatum) m]
                    -> m (ChanT.OutChanI IstatdDatum, ChanT.InChanI a)
mkPipeEnvWithSource source ps = do
  (inC, outC) <- newZChan
  sink <- mkPipeRecorder inC
  fp <- mkFilterPipelineWithSource sink source ps
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
        ensureNoLeftovers tchan
    it "adds prefixes to the counter in order" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterPrefix "c"
                                       , mkFilterPrefix "b"
                                       , mkFilterPrefix "a"
                                       ]
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "cbaCounterName" 0 1)
        ensureNoLeftovers tchan
    it "adds suffix to the counter" $ do
        (tchan, pipeline) <- mkPipeEnv [mkFilterSuffix "a"]
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "CounterNamea" 0 1)
        ensureNoLeftovers tchan
    it "adds suffixes to the counter in order" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterSuffix "c"
                                       , mkFilterSuffix "b"
                                       , mkFilterSuffix "a"
                                       ]
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "CounterNameabc" 0 1)
        ensureNoLeftovers tchan
    it "adds prefix and suffix to the counter" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterSuffix "b"
                                       , mkFilterPrefix "a"
                                       ]
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "aCounterNameb" 0 1)
        ensureNoLeftovers tchan
    it "adds suffix and prefix to the counter" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterPrefix "b"
                                       , mkFilterSuffix "a"
                                       ]
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "bCounterNamea" 0 1)
        ensureNoLeftovers tchan
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
        ensureNoLeftovers tchan
    it "sends all messages through channel" $ do
        (tchan, pipeline) <- mkPipeEnv []
        replicateM_ 100 $ writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        replicateM_ 100 $ do
            res <- readChan tchan
            res `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
        ensureNoLeftovers tchan
    it "sends all messages through channel of 1000 buffering 100 messages" $ do
        (tchan, pipeline) <- mkPipeEnv [mkBuffer 1000]
        replicateM_ 100 $ writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        replicateM_ 100 $ do
            res <- readChan tchan
            res `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
        ensureNoLeftovers tchan
    it "sends all messages through channel of 1000 buffering 10000 messages" $ do
        (tchan, pipeline) <- mkPipeEnv [mkBuffer 1000]
        replicateM_ 10000 $ writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        replicateM_ 10000 $ do
            res <- readChan tchan
            res `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
        ensureNoLeftovers tchan
    it "difference counters maintain state" $ do
        dstate <- mkDifferenceState
        (tchan, pipeline) <- mkPipeEnvWithSource (mkFilterDifference dstate) []
        forM_ [1..10000] $ \v -> writeChan pipeline $ DifferenceCounter "CounterName" 0 v
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "CounterName" 0 0)
        replicateM_ 9999 $ do
            res <- readChan tchan
            res `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
        ensureNoLeftovers tchan
    it "difference counters maintain state with other filters" $ do
        dstate <- mkDifferenceState
        (tchan, pipeline) <- mkPipeEnvWithSource (mkFilterDifference dstate) [mkFilterPrefix "a", mkFilterSuffix "c", mkBuffer 1000]
        forM_ [1..10000] $ \v -> writeChan pipeline $ DifferenceCounter "CounterName" 0 v
        res <- readChan tchan
        res `shouldBe` (IstatdDatum Counter "aCounterNamec" 0 0)
        replicateM_ 9999 $ do
            res <- readChan tchan
            res `shouldBe` (IstatdDatum Counter "aCounterNamec" 0 1)
        ensureNoLeftovers tchan


ensureNoLeftovers c = do
    res <- (readChan c >> return False) `catch` \(_ :: ChannelException) -> return True
    res `shouldBe` True
