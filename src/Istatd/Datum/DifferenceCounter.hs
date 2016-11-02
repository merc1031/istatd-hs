{-# LANGUAGE RecordWildCards #-}
module Istatd.Datum.DifferenceCounter
( DifferenceState (..)
, mkDifferenceState
, computeDifferenceCounter
, mkDifferenceCounter
)
where

import            Control.Concurrent.STM
import            Control.Monad.IO.Class              ( MonadIO
                                                      , liftIO
                                                      )
import            Istatd.Datum.Types                  ( IstatdDatum (..)
                                                      , Counter (..)
                                                      )

import qualified  Data.ByteString.Lazy.Builder        as BSLB
import qualified  Data.ByteString.Lazy.Char8          as BSLC
import qualified  Data.HashMap.Strict                 as HM
import qualified  Data.Time.Clock.POSIX               as POSIX

--data DifferenceCounter  = DifferenceCounter !BSLC.ByteString !POSIX.POSIXTime !Double
newtype DifferenceState = DifferenceState (TVar (HM.HashMap BSLC.ByteString Double))

mkDifferenceCounter state k v =
  let a = IstatdDatum { _getKey = k
                      , _getKeyC = BSLB.toLazyByteString k
                      , _updateKey = \nk -> a { _getKey = nk, _getKeyC = BSLB.toLazyByteString nk }
                      , _compute = computeDifferenceCounter state a
                      }
  in a

mkDifferenceState
  :: MonadIO m
  => m DifferenceState
mkDifferenceState =
  DifferenceState <$> (liftIO $ newTVarIO HM.empty)

computeDifferenceCounter
  :: MonadIO m
  => DifferenceState
  -> IstatdDatum
  -> m Counter
computeDifferenceCounter (DifferenceState stateV) (IstatdDatum {..}) = do
  v' <- liftIO $ atomically $ do
    oldH <- readTVar stateV
    let diff = case HM.lookup k oldH of
          Just d -> v - d
          Nothing -> 0
    modifyTVar' stateV (\h -> HM.insert k v h)
    return diff
  return $ Counter (BSLB.lazyByteString k) t v'
