{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Istatd.Datum.DifferenceCounter
( DifferenceCounter (..)
, DifferenceState (..)
, mkDifferenceState
, computeDifferenceCounter
)
where

import            Control.Concurrent.STM
import            Control.Monad.IO.Class              ( MonadIO
                                                      , liftIO
                                                      )
import            Istatd.Types                        ( IstatdDatum (..)
                                                      , IstatdType (..)
                                                      , HasKey (..)
                                                      )
import Istatd.TypeSet (Cmp)

import qualified  Data.ByteString.Lazy.Builder        as BSLB
import qualified  Data.ByteString.Lazy.Char8          as BSLC
import qualified  Data.HashMap.Strict                 as HM
import qualified  Data.Time.Clock.POSIX               as POSIX

data DifferenceCounter  = DifferenceCounter !BSLC.ByteString !POSIX.POSIXTime !Double
newtype DifferenceState = DifferenceState (TVar (HM.HashMap BSLC.ByteString Double))

instance HasKey DifferenceCounter where
  getKey (DifferenceCounter k _ _) = BSLB.lazyByteString k
  updateKey (DifferenceCounter _k t v) k = DifferenceCounter (BSLB.toLazyByteString k) t v

type instance Cmp DifferenceCounter IstatdDatum = 'GT
type instance Cmp IstatdDatum DifferenceCounter = 'LT
type instance Cmp DifferenceCounter DifferenceCounter = 'EQ

mkDifferenceState
  :: MonadIO m
  => m DifferenceState
mkDifferenceState =
  DifferenceState <$> (liftIO $ newTVarIO HM.empty)

computeDifferenceCounter
  :: MonadIO m
  => DifferenceState
  -> DifferenceCounter
  -> m IstatdDatum
computeDifferenceCounter (DifferenceState stateV) (DifferenceCounter k t v) = do
  v' <- liftIO $ atomically $ do
    oldH <- readTVar stateV
    let diff = case HM.lookup k oldH of
          Just d -> v - d
          Nothing -> 0
    modifyTVar' stateV (\h -> HM.insert k v h)
    return diff
  return $ IstatdDatum Counter (BSLB.lazyByteString k) t v'
