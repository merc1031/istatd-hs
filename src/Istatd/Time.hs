{-# LANGUAGE FlexibleInstances #-}
module Istatd.Time
( SupportsTime(..)
)
where

import qualified  Data.Time.Clock.POSIX           as POSIX
import qualified  Control.Concurrent.Chan.Unagi   as U

class Monad m => SupportsTime m where
    getPOSIXTime :: m POSIX.POSIXTime
    tick :: Int -> m (U.OutChan ())
    threadDelay :: Int -> m ()
