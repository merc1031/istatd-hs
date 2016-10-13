{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Istatd where

import Control.Monad (void, forever, (=<<), (>=>))
import Data.Foldable (foldrM)
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

data IstatChan = ZChan (U.InChan Rec)
               | BChan (BU.InChan Rec)

iWriteChan :: IstatChan -> Rec -> IO ()
iWriteChan (ZChan uchan) r = U.writeChan uchan r
iWriteChan (BChan buchan) r = BU.writeChan buchan r

--iReadChan :: IstatChan -> IO Rec
--iReadChan (ZChan uchan) = U.readChan uchan
--iReadChan (BChan buchan) = BU.readChan buchan

getKey :: Rec -> TL.Text
getKey (Counter k _ _) = k
getKey (Gauge k _ _) = k

updateKey :: TL.Text -> Rec -> Rec
updateKey k (Counter _ t i) = Counter k t i
updateKey k (Gauge _ t d) = Gauge k t d

type FilterFunc = (IstatChan -> IO IstatChan)

mkFilterPipeline :: IstatChan -> [FilterFunc] -> IO IstatChan
mkFilterPipeline sink fs = foldr (>=>) return fs $ sink

mkFilterPrefix :: TL.Text -> FilterFunc
mkFilterPrefix prefix = \out -> do
    (inC, outC) <- U.newChan
    let action r@(getKey -> k) = let nk = TL.append prefix k
                                 in iWriteChan out $ updateKey nk r
    void $ async $ forever $ action =<< U.readChan outC
    return $ ZChan inC

mkFilterSuffix :: TL.Text -> FilterFunc
mkFilterSuffix suffix = \out -> do
    (inC, outC) <- U.newChan
    let action r@(getKey -> k) = let nk = TL.append k suffix
                                 in iWriteChan out $ updateKey nk r
    void $ async $ forever $ action =<< U.readChan outC
    return $ ZChan inC

mkBuffer :: Int -> FilterFunc
mkBuffer size = \out -> do
    (inC, outC) <- BU.newChan size
    let action = void $ async $ forever $ iWriteChan out =<< BU.readChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> return ()
    return $ BChan inC

mkMonitoredBuffer :: TL.Text -> Int -> FilterFunc
mkMonitoredBuffer name size = \out -> do
    (inC, outC) <- BU.newChan size
    let action = void $ async $ forever $ iWriteChan out =<< BU.readChan outC
    let recordAction = void $ async $ forever $ (iWriteChan out =<< BU.readChan outC) >> threadDelay 1000000
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> return ()
    return $ BChan inC

mkNullRecorder :: IO IstatChan
mkNullRecorder = do
    (inC, outC) <- U.newChan
    let action = void $ async $ forever $ void $ U.readChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> return ()
    return $ ZChan inC

mkPrintingRecorder :: IO IstatChan
mkPrintingRecorder = do
    (inC, outC) <- U.newChan
    let action = void $ async $ forever $ putStrLn . show =<< U.readChan outC
    action `catch` \(_ :: BlockedIndefinitelyOnMVar) -> return ()
    return $ ZChan inC


--data Ticker = Ticker {
--      channel :: U.OutChan ()
--    , 
--newTicker :: Int -> IO Ticker
--
--tick :: Int -> IO (U.OutChan ())
--tick d = do
--    (_, o) <- U.newChan
--    return o
