{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Istatd where

import Control.Arrow ((***))
import Control.Monad (void, forever, (=<<), (>=>))
import Data.Foldable (foldrM)
import Data.IORef
import Control.Debounce (mkDebounce, defaultDebounceSettings, DebounceSettings (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Exception.Safe (catch)
import Control.Exception (BlockedIndefinitelyOnMVar)
import Control.Monad.IO.Class
import Control.Concurrent.STM.TBMChan
import qualified Control.Concurrent.Chan.Unagi as U
import qualified Control.Concurrent.Chan.Unagi.Bounded as BU
import qualified Data.Text.Lazy as TL

data Rec = Counter TL.Text Int Int
         | Gauge TL.Text Int Double
         deriving (Show)

data IstatInChan = ZInChan (U.InChan Rec)
                 | BInChan (IORef Int) (BU.InChan Rec)

data IstatOutChan = ZOutChan (U.OutChan Rec)
                  | BOutChan (IORef Int) (BU.OutChan Rec)

iWriteChan :: IstatInChan -> Rec -> IO ()
iWriteChan (ZInChan uchan) r = U.writeChan uchan r
iWriteChan (BInChan c buchan) r = BU.writeChan buchan r >> (void $ atomicModifyIORef' c (\c' -> (c' + 1, ())))

iReadChan :: IstatOutChan -> IO Rec
iReadChan (ZOutChan uchan) = U.readChan uchan
iReadChan (BOutChan c buchan) = BU.readChan buchan >>= \r -> (void $ atomicModifyIORef' c (\c' -> (c' - 1, ()))) >> return r

iOutChanLen :: IstatOutChan -> IO Int
iOutChanLen (ZOutChan {}) = return 0
iOutChanLen (BOutChan c _) = atomicModifyIORef' c (\c' -> (c', c'))

iInChanLen :: IstatInChan -> IO Int
iInChanLen (ZInChan {}) = return 0
iInChanLen (BInChan c _) = atomicModifyIORef' c (\c' -> (c', c'))

newZChan :: IO (IstatInChan, IstatOutChan)
newZChan = (ZInChan *** ZOutChan) <$> U.newChan

newBChan :: Int -> IO (IstatInChan, IstatOutChan)
newBChan size = do
  c <- newIORef 0
  (BInChan c *** BOutChan c) <$> BU.newChan size

getKey :: Rec -> TL.Text
getKey (Counter k _ _) = k
getKey (Gauge k _ _) = k

updateKey :: TL.Text -> Rec -> Rec
updateKey k (Counter _ t i) = Counter k t i
updateKey k (Gauge _ t d) = Gauge k t d

type FilterFunc = (IstatInChan -> IO IstatInChan)

mkFilterPipeline :: IstatInChan -> [FilterFunc] -> IO IstatInChan
mkFilterPipeline sink fs = foldr (>=>) return fs $ sink

mkFilterPrefix :: TL.Text -> FilterFunc
mkFilterPrefix prefix = \out -> do
    (inC, outC) <- newZChan
    let action r@(getKey -> k) = let nk = TL.append prefix k
                                 in iWriteChan out $ updateKey nk r
    void $ async $ forever $ action =<< iReadChan outC
    return inC

mkFilterSuffix :: TL.Text -> FilterFunc
mkFilterSuffix suffix = \out -> do
    (inC, outC) <- newZChan
    let action r@(getKey -> k) = let nk = TL.append k suffix
                                 in iWriteChan out $ updateKey nk r
    void $ async $ forever $ action =<< iReadChan outC
    return inC

mkBuffer :: Int -> FilterFunc
mkBuffer size = \out -> do
    (inC, outC) <- newBChan size
    let action = void $ async $ forever $ iWriteChan out =<< iReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLn "Buffer died" >> return ()
    return inC

mkMonitoredBuffer :: TL.Text -> Int -> FilterFunc
mkMonitoredBuffer name size = \out -> do
    (inC, outC) <- newBChan size
    let action = void $ async $ forever $ iWriteChan out =<< (iReadChan outC)
    ticker <- tick 1
    let recordAction = void $ async $ forever $ U.readChan ticker >> ((\c'' -> (iWriteChan out $ Gauge name 1 (fromIntegral c''))) =<< iInChanLen inC)
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLn "MonitoredBuffer died" >> return ()
    recordAction
    return inC

mkSlowdownBuffer :: Int -> Int -> FilterFunc
mkSlowdownBuffer time size = \out -> do
    (inC, outC) <- newBChan size
    let action = void $ async $ forever $ iWriteChan out =<< (iReadChan outC << threadDelay time)
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLn "MonitoredBuffer died" >> return ()
    return inC

mkNullRecorder :: IO IstatInChan
mkNullRecorder = do
    (inC, outC) <- newZChan
    let action = void $ async $ forever $ void $ iReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLn "Recorder died" >> return ()
    return inC

mkPrintingRecorder :: IO IstatInChan
mkPrintingRecorder = do
    (inC, outC) <- newZChan
    let action = void $ async $ forever $ putStrLn . show =<< iReadChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> putStrLn "Recorder died" >> return ()
    return inC


tick :: Int -> IO (U.OutChan ())
tick d = do
    (i, o) <- U.newChan
    let action = U.writeChan i ()
    void $ async $ forever $ action >> threadDelay (d * 1000000)
    return o

(<<) :: Monad m => m b -> m () -> m b
a << s = const a =<< s

