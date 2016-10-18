{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Istatd.IstatdSpec where

import            Control.Exception                 ( catch )
import            Control.Monad                     ( replicateM_
                                                    , replicateM
                                                    , forM_
                                                    )
import            Control.Monad.Catch               ( MonadCatch
                                                    , MonadThrow
                                                    )
import            Control.Monad.Reader
import            Control.Monad.IO.Class            ( MonadIO )
import            Istatd.Istatd
import            Test.Hspec
import qualified  Istatd.Chan.Chan                  as Chan
import qualified  Istatd.Chan.ChanT                 as ChanT
import qualified  Control.Concurrent.Chan.Unagi   as U
import qualified  Data.Time.Clock.POSIX as POSIX

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

data FakeState = FakeState
    { fakeTimer :: U.OutChan ()
    , fakeTime :: POSIX.POSIXTime
    }

newtype Fake a = Fake { runFake :: ReaderT FakeState IO a }
             deriving (Functor, Applicative, Monad, MonadIO, MonadReader FakeState, MonadCatch, MonadThrow)

instance SupportsTime Fake where
    tick _ = asks fakeTimer
    getPOSIXTime = asks fakeTime

runFakeM state a = runReaderT (runFake a) state
runFakeMIO stateA a = stateA >>= \state -> runReaderT (runFake a) state

defaultFakeTimerIO = snd <$> U.newChan
newFakeStateIO tIO time = tIO >>= \t -> return FakeState { fakeTimer = t, fakeTime = time }
newFakeState t time = FakeState { fakeTimer = t, fakeTime = time }

spec :: Spec
spec = do
  describe "filterPrefix" $ do
    it "adds a prefix to the counter" $ do
        res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0) $ do
            (tchan, pipeline) <- mkPipeEnv [mkFilterPrefix "a"]
            writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
            res <- readChan tchan
            ensureNoLeftovers tchan
            return res
        res `shouldBe` (IstatdDatum Counter "aCounterName" 0 1)
    it "adds prefixes to the counter in order" $ do
        res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0) $ do
            (tchan, pipeline) <- mkPipeEnv [ mkFilterPrefix "c"
                                        , mkFilterPrefix "b"
                                        , mkFilterPrefix "a"
                                        ]
            writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
            res <- readChan tchan
            ensureNoLeftovers tchan
            return res
        res `shouldBe` (IstatdDatum Counter "cbaCounterName" 0 1)
    it "adds suffix to the counter" $ do
        res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0) $ do
            (tchan, pipeline) <- mkPipeEnv [mkFilterSuffix "a"]
            writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
            res <- readChan tchan
            ensureNoLeftovers tchan
            return res
        res `shouldBe` (IstatdDatum Counter "CounterNamea" 0 1)
    it "adds suffixes to the counter in order" $ do
        res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0) $ do
            (tchan, pipeline) <- mkPipeEnv [ mkFilterSuffix "c"
                                        , mkFilterSuffix "b"
                                        , mkFilterSuffix "a"
                                        ]
            writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
            res <- readChan tchan
            ensureNoLeftovers tchan
            return res
        res `shouldBe` (IstatdDatum Counter "CounterNameabc" 0 1)
    it "adds prefix and suffix to the counter" $ do
        res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0) $ do
            (tchan, pipeline) <- mkPipeEnv [ mkFilterSuffix "b"
                                        , mkFilterPrefix "a"
                                        ]
            writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
            res <- readChan tchan
            ensureNoLeftovers tchan
            return res
        res `shouldBe` (IstatdDatum Counter "aCounterNameb" 0 1)
    it "adds suffix and prefix to the counter" $ do
        res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0) $ do
            (tchan, pipeline) <- mkPipeEnv [ mkFilterPrefix "b"
                                        , mkFilterSuffix "a"
                                        ]
            writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
            res <- readChan tchan
            ensureNoLeftovers tchan
            return res
        res `shouldBe` (IstatdDatum Counter "bCounterNamea" 0 1)
    it "adds suffixes and prefixes to the counter in order" $ do
        res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0) $ do
            (tchan, pipeline) <- mkPipeEnv [ mkFilterPrefix "f"
                                        , mkFilterSuffix "e"
                                        , mkFilterSuffix "d"
                                        , mkFilterPrefix "c"
                                        , mkFilterPrefix "b"
                                        , mkFilterSuffix "a"
                                        ]
            writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
            res <- readChan tchan
            ensureNoLeftovers tchan
            return res
        res `shouldBe` (IstatdDatum Counter "fcbCounterNameade" 0 1)
    it "sends all messages through channel" $ do
        res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0) $ do
            (tchan, pipeline) <- mkPipeEnv []
            replicateM_ 100 $ writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
            res <- replicateM 100 $ readChan tchan
            ensureNoLeftovers tchan
            return res
        forM_ res $ \r -> r `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
    it "sends all messages through channel of 1000 buffering 100 messages" $ do
        res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0) $ do
            (tchan, pipeline) <- mkPipeEnv [mkBuffer 1000]
            replicateM_ 100 $ writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
            res <- replicateM 100 $ readChan tchan
            ensureNoLeftovers tchan
            return res
        forM_ res $ \r -> r `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
    it "sends all messages through channel of 1000 buffering 10000 messages" $ do
        res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0) $ do
            (tchan, pipeline) <- mkPipeEnv [mkBuffer 1000]
            replicateM_ 10000 $ writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
            res <- replicateM 10000 $ readChan tchan
            ensureNoLeftovers tchan
            return res
        forM_ res $ \r -> r `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
    it "difference counters maintain state" $ do
        (res, res') <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0) $ do
            dstate <- mkDifferenceState
            (tchan, pipeline) <- mkPipeEnvWithSource (mkFilterDifference dstate) []
            forM_ [1..10000] $ \v -> writeChan pipeline $ DifferenceCounter "CounterName" 0 v
            res <- readChan tchan
            res' <- replicateM 9999 $ readChan tchan
            ensureNoLeftovers tchan
            return (res, res')
        res `shouldBe` (IstatdDatum Counter "CounterName" 0 0)
        forM_ res' $ \r -> r `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
    it "difference counters maintain state with other filters" $ do
        (res, res') <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0) $ do
            dstate <- mkDifferenceState
            (tchan, pipeline) <- mkPipeEnvWithSource (mkFilterDifference dstate) [mkFilterPrefix "a", mkFilterSuffix "c", mkBuffer 1000]
            forM_ [1..10000] $ \v -> writeChan pipeline $ DifferenceCounter "CounterName" 0 v
            res <- readChan tchan
            res' <- replicateM 9999 $ readChan tchan
            ensureNoLeftovers tchan
            return (res, res')
        res `shouldBe` (IstatdDatum Counter "aCounterNamec" 0 0)
        forM_ res' $ \r -> r `shouldBe` (IstatdDatum Counter "aCounterNamec" 0 1)


ensureNoLeftovers c = liftIO $ do
    res <- (readChan c >> return False) `catch` \(_ :: ChannelException) -> return True
    res `shouldBe` True
