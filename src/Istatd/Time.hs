{-# LANGUAGE FlexibleInstances #-}
module Istatd.Time
( SupportsTime(..)
)
where

--import            Control.Monad.IO.Class          ( MonadIO
--                                                  , liftIO
--                                                  )
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Istatd.Tick as Tick
import qualified  Control.Concurrent.Chan.Unagi   as U

class Monad m => SupportsTime m where
    getPOSIXTime :: m POSIX.POSIXTime
    tick :: Int -> m (U.OutChan ())

instance SupportsTime IO where
    getPOSIXTime = POSIX.getPOSIXTime
    tick = Tick.tick
