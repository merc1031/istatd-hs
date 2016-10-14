module Istatd.Tick
( tick
)
where

import            Control.Concurrent              ( threadDelay )
import            Control.Concurrent.Async        ( async )
import            Control.Monad                   ( void
                                                  , forever
                                                  )
import            Control.Monad.IO.Class          ( MonadIO
                                                  , liftIO
                                                  )

import qualified  Control.Concurrent.Chan.Unagi   as U

tick :: (MonadIO m)
     => Int
     -> m (U.OutChan ())
tick d = liftIO $ do
    (i, o) <- U.newChan
    let action = U.writeChan i ()
    void $ async $ forever $ action >> threadDelay (d * 1000000)
    return o

