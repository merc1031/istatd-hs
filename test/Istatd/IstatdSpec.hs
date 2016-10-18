{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Istatd.IstatdSpec where

import            Control.Concurrent.STM
import            Control.Exception                 ( catch )
import            Control.Monad                     ( replicateM_
                                                    , replicateM
                                                    , forM_
                                                    )
import            Control.Monad.Base                ( MonadBase )
import            Control.Monad.Catch               ( MonadCatch
                                                    , MonadThrow
                                                    )
import            Control.Monad.Reader
import            Control.Monad.IO.Class            ( MonadIO )
import            Control.Monad.Trans               ( MonadTrans (..) )
import            Control.Monad.Trans.Control       ( MonadBaseControl (..)
                                                    , MonadTransControl (..)
                                                    , ComposeSt
                                                    , defaultLiftBaseWith
                                                    , defaultRestoreM
                                                    )
import            Istatd.Istatd
import            Test.Hspec

import qualified  Control.Concurrent.Chan.Unagi     as U
import qualified  Data.Time.Clock.POSIX             as POSIX
import qualified  Istatd.Chan.Chan                  as Chan
import qualified  Istatd.Chan.ChanT                 as ChanT

mkPipeEnv'
  :: ( MonadIO m
     , MonadCatch m
     )
  => m (ChanT.OutChanI IstatdDatum, ChanT.InChanI IstatdDatum)
mkPipeEnv' = do
  (inC, outC) <- newZChan
  sink <- mkPipeRecorder inC
  return (outC, sink)

mkPipeEnv
  :: ( MonadIO m
     , MonadCatch m
     , InData d IstatdDatum
     )
  => FilterFuncT (ChanT.InChanI IstatdDatum) (ChanT.InChanI d) m
  -> m (ChanT.OutChanI IstatdDatum, ChanT.InChanI d)
mkPipeEnv ps = do
  (inC, outC) <- newZChan
  sink <- mkPipeRecorder inC
  fp <- ps sink
  return (outC, fp)

mkPipeEnvWithSource
  :: ( MonadIO m
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

data FakeState = FakeState
  { fakeTimer       :: U.OutChan ()
  , fakeTime        :: POSIX.POSIXTime
  , fakeThreadDelay :: Maybe (TMVar ())
  }

newtype Fake m a =
  Fake { runFake :: ReaderT FakeState m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader FakeState
           , MonadCatch
           , MonadThrow
           , MonadTrans
           , MonadBase b
           )

instance MonadIO m => SupportsTime (Fake m) where
  tick _ = asks fakeTimer
  getPOSIXTime = asks fakeTime
  threadDelay _ = do
      mThreadDelay <- asks fakeThreadDelay
      case mThreadDelay of
        Nothing -> return ()
        Just threadDelay -> liftIO $ atomically $ takeTMVar threadDelay

instance MonadTransControl Fake where
  type StT Fake a = a
  liftWith f = Fake $ ReaderT $ \le -> f $ \t -> runFakeM le t
  restoreT = Fake . ReaderT . const

instance (MonadBaseControl b m) => MonadBaseControl b (Fake m) where
  type StM (Fake m) a = ComposeSt Fake m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

runFakeM state a = runReaderT (runFake a) state

runFakeMIO stateA a = stateA >>= \state -> runReaderT (runFake a) state

defaultFakeTimerIO = snd <$> U.newChan

defaultThreadDelay = Nothing

defaultThreadDelayIO = return defaultThreadDelay

newFakeStateIO tIO time tdIO = tIO >>= \t -> tdIO >>= \td -> return FakeState { fakeTimer = t, fakeTime = time, fakeThreadDelay = td}

newFakeState t time td = FakeState { fakeTimer = t, fakeTime = time, fakeThreadDelay = td }

spec :: Spec
spec = do
  describe "filterPrefix" $ do
    it "adds a prefix to the counter" $ do
      res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        (tchan, pipeline) <- mkPipeEnv $ mkFilterPrefix "a"
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        ensureNoLeftovers tchan
        return res
      res `shouldBe` (IstatdDatum Counter "aCounterName" 0 1)
    it "adds prefixes to the counter in order" $ do
      res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        (tchan, pipeline) <- mkPipeEnv $ mkFilterPrefix "c"
                                    .:>. mkFilterPrefix "b"
                                    .:>. mkFilterPrefix "a"

        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        ensureNoLeftovers tchan
        return res
      res `shouldBe` (IstatdDatum Counter "cbaCounterName" 0 1)
    it "adds suffix to the counter" $ do
      res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        (tchan, pipeline) <- mkPipeEnv $ mkFilterSuffix "a"
        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        ensureNoLeftovers tchan
        return res
      res `shouldBe` (IstatdDatum Counter "CounterNamea" 0 1)
    it "adds suffixes to the counter in order" $ do
      res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        (tchan, pipeline) <- mkPipeEnv $ mkFilterSuffix "c"
                                    .:>. mkFilterSuffix "b"
                                    .:>. mkFilterSuffix "a"

        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        ensureNoLeftovers tchan
        return res
      res `shouldBe` (IstatdDatum Counter "CounterNameabc" 0 1)
    it "adds prefix and suffix to the counter" $ do
      res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        (tchan, pipeline) <- mkPipeEnv $ mkFilterSuffix "b"
                                    .:>. mkFilterPrefix "a"

        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        ensureNoLeftovers tchan
        return res
      res `shouldBe` (IstatdDatum Counter "aCounterNameb" 0 1)
    it "adds suffix and prefix to the counter" $ do
      res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        (tchan, pipeline) <- mkPipeEnv $ mkFilterPrefix "b"
                                    .:>. mkFilterSuffix "a"

        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        ensureNoLeftovers tchan
        return res
      res `shouldBe` (IstatdDatum Counter "bCounterNamea" 0 1)
    it "adds suffixes and prefixes to the counter in order" $ do
      res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        (tchan, pipeline) <- mkPipeEnv $ mkFilterPrefix "f"
                                    .:>. mkFilterSuffix "e"
                                    .:>. mkFilterSuffix "d"
                                    .:>. mkFilterPrefix "c"
                                    .:>. mkFilterPrefix "b"
                                    .:>. mkFilterSuffix "a"

        writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- readChan tchan
        ensureNoLeftovers tchan
        return res
      res `shouldBe` (IstatdDatum Counter "fcbCounterNameade" 0 1)
    it "sends all messages through channel" $ do
      res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        (tchan, pipeline) <- mkPipeEnv'
        replicateM_ 100 $ writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- replicateM 100 $ readChan tchan
        ensureNoLeftovers tchan
        return res
      forM_ res $ \r -> r `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
    it "sends all messages through channel of 1000 buffering 100 messages" $ do
      res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        (tchan, pipeline) <- mkPipeEnv $ mkBuffer 1000
        replicateM_ 100 $ writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- replicateM 100 $ readChan tchan
        ensureNoLeftovers tchan
        return res
      forM_ res $ \r -> r `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
    it "sends all messages through channel of 1000 buffering 10000 messages" $ do
      res <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        (tchan, pipeline) <- mkPipeEnv $ mkBuffer 1000
        replicateM_ 10000 $ writeChan pipeline $ IstatdDatum Counter "CounterName" 0 1
        res <- replicateM 10000 $ readChan tchan
        ensureNoLeftovers tchan
        return res
      forM_ res $ \r -> r `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
    it "difference counters maintain state" $ do
      (res, res') <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        dstate <- mkDifferenceState
        (tchan, pipeline) <- mkPipeEnv $ mkFilterDifference dstate
        forM_ [1..10000] $ \v -> writeChan pipeline $ DifferenceCounter "CounterName" 0 v
        res <- readChan tchan
        res' <- replicateM 9999 $ readChan tchan
        ensureNoLeftovers tchan
        return (res, res')
      res `shouldBe` (IstatdDatum Counter "CounterName" 0 0)
      forM_ res' $ \r -> r `shouldBe` (IstatdDatum Counter "CounterName" 0 1)
    it "difference counters maintain state with other filters" $ do
      (res, res') <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        dstate <- mkDifferenceState
        (tchan, pipeline) <- mkPipeEnv $ mkFilterDifference dstate .<:. mkFilterPrefix "a" .:>. mkFilterSuffix "c" .:>. mkBuffer 1000
        forM_ [1..10000] $ \v -> writeChan pipeline $ DifferenceCounter "CounterName" 0 v
        res <- readChan tchan
        res' <- replicateM 9999 $ readChan tchan
        ensureNoLeftovers tchan
        return (res, res')
      res `shouldBe` (IstatdDatum Counter "aCounterNamec" 0 0)
      forM_ res' $ \r -> r `shouldBe` (IstatdDatum Counter "aCounterNamec" 0 1)
    it "difference counters don't need to be first?" $ do
      (res, res') <- runFakeMIO (newFakeStateIO defaultFakeTimerIO 0 defaultThreadDelayIO) $ do
        dstate <- mkDifferenceState
        (tchan, pipeline) <- mkPipeEnv $ mkFilterDifference dstate .:>. mkFilterPrefix "a" .:>. mkFilterSuffix "c" .:>. mkBuffer 1000
        forM_ [1..10000] $ \v -> writeChan pipeline $ DifferenceCounter "CounterName" 0 v
        res <- readChan tchan
        res' <- replicateM 9999 $ readChan tchan
        ensureNoLeftovers tchan
        return (res, res')
      res `shouldBe` (IstatdDatum Counter "aCounterNamec" 0 0)
      forM_ res' $ \r -> r `shouldBe` (IstatdDatum Counter "aCounterNamec" 0 1)
    it "percentile counters report for gauges" $ do
      (timerIn, timer) <- U.newChan
      let state = newFakeState timer 0 defaultThreadDelay

      (res, res1, res10, res100) <- runFakeM state $ do
        let ps = mkPercentiles [1, 10, 100]
        (tchan, pipeline) <- mkPipeEnv $ mkPercentileFilter 1 ps
        forM_ [0..99] $ \v -> writeChan pipeline $ IstatdDatum Gauge "CounterName" 0 v
        res <- replicateM 100 $ readChan tchan
        liftIO $ U.writeChan timerIn ()
        res1 <- readChan tchan
        res10 <- readChan tchan
        res100 <- readChan tchan
        ensureNoLeftovers tchan
        return (res, res1, res10, res100)
      forM_ (zip [0..99] res) $ \(v, r) -> r `shouldBe` (IstatdDatum Gauge "CounterName.raw" 0 v)
      res1 `shouldBe` (IstatdDatum Gauge "CounterName.1th" 0 1)
      res10 `shouldBe` (IstatdDatum Gauge "CounterName.10th" 0 10)
      res100 `shouldBe` (IstatdDatum Gauge "CounterName.100th" 0 99)


ensureNoLeftovers c = liftIO $ do
  res <- (readChan c >> return False) `catch` \(_ :: ChannelException) -> return True
  res `shouldBe` True

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

