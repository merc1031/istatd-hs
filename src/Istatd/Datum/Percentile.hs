{-# LANGUAGE OverloadedStrings #-}
module Istatd.Datum.Percentile
( PercentileState (..)
, Percentile
, mkPercentile
, mkPercentiles
, mkPercentileState
, clearPercentiles
, computePercentile
, computePercentiles
, addPercentile
)
where

import            Control.Concurrent.STM
import            Control.Monad                   ( forM )
import            Control.Monad.IO.Class          ( MonadIO
                                                  , liftIO
                                                  )
import            Data.Maybe                      ( catMaybes )
import            Data.Monoid                     ( (<>) )
import            Istatd.Class.Time               ( SupportsTime (..) )
import            Istatd.Types                    ( IstatdDatum (..)
                                                  , IstatdType (..)
                                                  )

import qualified  Data.ByteString.Lazy            as BSLC
import qualified  Data.ByteString.Lazy.Builder    as BSLB
import qualified  Data.HashMap.Strict             as HM
import qualified  Data.Sequence                   as Seq

newtype Percentile      = Percentile Int
newtype PercentileState = PercentileState (TVar (HM.HashMap BSLC.ByteString (Seq.Seq Double)))

mkPercentiles
  :: [Int]
  -> [Percentile]
mkPercentiles is =
  catMaybes $ map mkPercentile is

mkPercentile
  :: Int
  -> Maybe Percentile
mkPercentile i
  | i >= 0 && i <= 100 = Just $ Percentile i
  | otherwise = Nothing


mkPercentileState
  :: MonadIO m
  => m PercentileState
mkPercentileState =
  PercentileState <$> (liftIO $ newTVarIO HM.empty)

clearPercentiles
  :: MonadIO m
  => PercentileState
  -> m ()
clearPercentiles (PercentileState stateV) =
  liftIO $ atomically $ modifyTVar' stateV $ const HM.empty

newtype Sorted a = Sorted (Seq.Seq a)

computePercentile
  :: ( MonadIO m
     , SupportsTime m
     )
  => PercentileState
  -> [Percentile]
  -> BSLC.ByteString
  -> m [IstatdDatum]
computePercentile (PercentileState stateV) percentiles k = do
  samples <- liftIO $ atomically $ do
    oldH <- readTVar stateV
    let s = case HM.lookup k oldH of
          Just ds -> ds
          Nothing -> Seq.empty
    return s

  t <- getPOSIXTime

  let tooFew :: Int -> Int -> Bool
      tooFew p l = l == 0 || (p < (100 `div` l))
      getPercentile (Sorted ss) (Percentile p) =
        let l = Seq.length ss
        in if tooFew p l
            then Nothing
            else let index = ceiling (fromIntegral (l * p) / (100 :: Double)) - 1
                 in mkDatum p <$> ss Seq.!? index
      mkDatum p v = IstatdDatum Gauge (BSLB.lazyByteString k <> BSLB.char7 '.' <> BSLB.intDec p <> BSLB.lazyByteString "th") t v

      sorted = Sorted $ Seq.sort samples
      ctrs = catMaybes $ map (getPercentile sorted) percentiles
  return ctrs

computePercentiles
  :: ( MonadIO m
     , SupportsTime m
     )
  => PercentileState
  -> [Percentile]
  -> m [IstatdDatum]
computePercentiles pstate@(PercentileState stateV) percentiles = do
  keys <- liftIO $ atomically $ do
    oldH <- readTVar stateV
    return $ HM.keys oldH

  ctrs <- forM keys $ \k -> computePercentile pstate percentiles k
  return $ concat ctrs

addPercentile
  :: MonadIO m
  => PercentileState
  -> BSLC.ByteString
  -> Double
  -> m ()
addPercentile (PercentileState stateV) k v = do
  liftIO $ atomically $ do
    oldH <- readTVar stateV
    let s = case HM.lookup k oldH of
          Just ds -> ds Seq.|> v
          Nothing -> Seq.empty
    modifyTVar' stateV $ HM.insert k s
