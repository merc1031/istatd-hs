{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Istatd.Monad.Types
( AppT (AppT)
, State (..)
, runAppM
, runAppM'
)
where

import            Control.Monad.Base                ( MonadBase )
import            Control.Monad.Catch               ( MonadCatch
                                                    , MonadThrow
                                                    )
import            Control.Monad.IO.Class            ( MonadIO )
import            Control.Monad.Reader
import            Control.Monad.Trans               ( MonadTrans (..) )
import            Control.Monad.Trans.Control       ( MonadBaseControl (..)
                                                    , MonadTransControl (..)
                                                    , ComposeSt
                                                    , defaultLiftBaseWith
                                                    , defaultRestoreM
                                                    )
import            Istatd.Class.Time                 ( SupportsTime (..) )

import qualified  Control.Concurrent                as Conc
import qualified  Istatd.Tick                       as Tick
import qualified  Data.Time.Clock.POSIX             as POSIX

data State = State

newtype AppT m a =
  AppT { runAppT :: ReaderT State m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader State
           , MonadCatch
           , MonadThrow
           , MonadTrans
           , MonadBase b
           )

instance MonadIO m => SupportsTime (AppT m) where
  tick         = AppT . Tick.tick
  getPOSIXTime = AppT $ liftIO POSIX.getPOSIXTime
  threadDelay  = AppT . liftIO . Conc.threadDelay

instance MonadTransControl AppT where
  type StT AppT a = a
  liftWith f      = AppT $ ReaderT $ \le -> f $ \t -> runAppM le t
  {-# INLINABLE liftWith #-}
  restoreT        = AppT . ReaderT . const
  {-# INLINABLE restoreT #-}

instance (MonadBaseControl b m) => MonadBaseControl b (AppT m) where
  type StM (AppT m) a = ComposeSt AppT m a
  liftBaseWith        = defaultLiftBaseWith
  {-# INLINABLE liftBaseWith #-}
  restoreM            = defaultRestoreM
  {-# INLINABLE restoreM #-}

runAppM
  :: State
  -> AppT m a
  -> m a
runAppM state a =
  runReaderT (runAppT a) state

runAppM'
  :: AppT m a
  -> m a
runAppM' a =
  runReaderT (runAppT a) State
