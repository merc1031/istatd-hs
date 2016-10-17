{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Istatd.IstatdSpec where

import Control.Exception (catch)
import Control.Monad (replicateM_
                     , forM_
                     )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadCatch)
import Test.Hspec
import Istatd.Istatd
import qualified Data.ByteString as BS
import qualified Istatd.Chan.ChanT as ChanT
import qualified Istatd.Chan.Chan as Chan

mkPipeEnv :: ( MonadIO m
             , MonadCatch m
             )
          => [FilterFunc (ChanT.InChanI IstatdDatum) m]
          -> m (ChanT.OutChanI IstatdDatum, ChanT.InChanI IstatdDatum)
mkPipeEnv ps = do
  (inC, outC) <- newZChan
  state <- mkSinkState
  sink <- mkPipeRecorder inC state
  fp <- mkFilterPipeline sink ps
  return (outC, fp)

mkPipeEncEnv :: ( MonadIO m
             , MonadCatch m
             )
          => [FilterFunc (ChanT.InChanI IstatdDatum) m]
          -> m (ChanT.OutChanI BS.ByteString, ChanT.InChanI IstatdDatum)
mkPipeEncEnv ps = do
  (inC, outC) <- newZChan
  state <- mkSinkState
  sink <- mkPipeEncodedRecorder inC state
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
        writeChan pipeline $ Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (Counter "aCounterName" 0 1)
        ensureNoLeftovers tchan
    it "adds prefixes to the counter in order" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterPrefix "c"
                                       , mkFilterPrefix "b"
                                       , mkFilterPrefix "a"
                                       ]
        writeChan pipeline $ Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (Counter "cbaCounterName" 0 1)
        ensureNoLeftovers tchan
    it "adds suffix to the counter" $ do
        (tchan, pipeline) <- mkPipeEnv [mkFilterSuffix "a"]
        writeChan pipeline $ Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (Counter "CounterNamea" 0 1)
        ensureNoLeftovers tchan
    it "adds suffixes to the counter in order" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterSuffix "c"
                                       , mkFilterSuffix "b"
                                       , mkFilterSuffix "a"
                                       ]
        writeChan pipeline $ Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (Counter "CounterNameabc" 0 1)
        ensureNoLeftovers tchan
    it "adds prefix and suffix to the counter" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterSuffix "b"
                                       , mkFilterPrefix "a"
                                       ]
        writeChan pipeline $ Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (Counter "aCounterNameb" 0 1)
        ensureNoLeftovers tchan
    it "adds suffix and prefix to the counter" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterPrefix "b"
                                       , mkFilterSuffix "a"
                                       ]
        writeChan pipeline $ Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (Counter "bCounterNamea" 0 1)
        ensureNoLeftovers tchan
    it "adds suffixes and prefixes to the counter in order" $ do
        (tchan, pipeline) <- mkPipeEnv [ mkFilterPrefix "f"
                                       , mkFilterSuffix "e"
                                       , mkFilterSuffix "d"
                                       , mkFilterPrefix "c"
                                       , mkFilterPrefix "b"
                                       , mkFilterSuffix "a"
                                       ]
        writeChan pipeline $ Counter "CounterName" 0 1
        res <- readChan tchan
        res `shouldBe` (Counter "fcbCounterNameade" 0 1)
        ensureNoLeftovers tchan
    it "sends all messages through channel" $ do
        (tchan, pipeline) <- mkPipeEnv []
        replicateM_ 100 $ writeChan pipeline $ Counter "CounterName" 0 1
        replicateM_ 100 $ do
            res <- readChan tchan
            res `shouldBe` (Counter "CounterName" 0 1)
        ensureNoLeftovers tchan
    it "sends all messages through channel buffering" $ do
        (tchan, pipeline) <- mkPipeEnv [mkBuffer 1000]
        replicateM_ 100 $ writeChan pipeline $ Counter "CounterName" 0 1
        replicateM_ 100 $ do
            res <- readChan tchan
            res `shouldBe` (Counter "CounterName" 0 1)
        ensureNoLeftovers tchan
    it "sends all messages through channel buffering" $ do
        (tchan, pipeline) <- mkPipeEnv [mkBuffer 1000]
        replicateM_ 10000 $ writeChan pipeline $ Counter "CounterName" 0 1
        replicateM_ 10000 $ do
            res <- readChan tchan
            res `shouldBe` (Counter "CounterName" 0 1)
        ensureNoLeftovers tchan
    it "works with difference counters" $ do
        (tchan, pipeline) <- mkPipeEncEnv [mkFilterPrefix "a"]
        forM_ [1..10000] $ \v -> writeChan pipeline $ Difference "CounterName" 0 v
        replicateM_ 10000 $ do
          res <- readChan tchan
          res `shouldBe` (Difference "aCounterName" 0 1)
        ensureNoLeftovers tchan


ensureNoLeftovers c = do
    res <- (readChan c >> return False) `catch` \(_ :: ChannelException) -> return True
    res `shouldBe` True
