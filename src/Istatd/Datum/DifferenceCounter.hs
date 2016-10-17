module Istatd.Datum.DifferenceCounter
( DifferenceCounter (..)
, DifferenceState (..)
, mkDifferenceState
)
where

import            Control.Concurrent.STM
import            Control.Monad.IO.Class              ( MonadIO
                                                      , liftIO
                                                      )

import qualified  Data.ByteString.Lazy.Char8          as BSLC
import qualified  Data.HashMap.Strict                 as HM
import qualified  Data.Time.Clock.POSIX               as POSIX

data DifferenceCounter = DifferenceCounter !BSLC.ByteString !POSIX.POSIXTime !Double
newtype DifferenceState = DifferenceState (TVar (HM.HashMap BSLC.ByteString Double))

mkDifferenceState :: MonadIO m
                  => m DifferenceState
mkDifferenceState = DifferenceState <$> (liftIO $ newTVarIO HM.empty)
