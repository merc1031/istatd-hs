{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Istatd.Chan.ChanLike where

import          Control.Monad.IO.Class      ( MonadIO )
import          Control.Exception           ( Exception )
import          Data.Typeable               ( Typeable )
import Istatd.Complexity

data ChannelException = ChannelBlockedException
                      deriving (Show, Typeable)

instance Exception ChannelException

data Sendable s f = Sendable s

instance (Functor (Sendable s)) where
  fmap _ (Sendable s) = Sendable s

mkSendable :: s -> Sendable s f
mkSendable s = Sendable s

unSendable :: Sendable s f -> s
unSendable (Sendable s) = s


-- | `ChanLike` implements an interface for channels that want to talk about
-- in and out sides. They are associated via a cyclical functional dependency.
-- In this way a function using an in(`cit`) or out(`cot`) channel of the correct `a`
-- may bring the type of the associated other end into scope.
-- This interface allows the generalized code to not worry about the type of the
-- specific channel or its underlying concurrency implementation.
class ChanLike (cit :: * -> *) (cot :: * -> *) (as :: [* -> *]) | cit as -> cot as, cot as -> cit as where
  newZChan
    :: MonadIO m
    => m (cit (Summed as (Sendable a f)), cot (Summed as (Sendable a f)))
  newBChan
    :: MonadIO m
    => Int
    -> m (cit (Summed as (Sendable a f)), cot (Summed as (Sendable a f)))

  writeChan
    :: forall a m f
     . ( MonadIO m
       , Sendable a :<: as
       )
    => (cit (Summed as (Sendable a f)))
    -> a
    -> m ()
  readChan
    :: forall a m f
     . ( MonadIO m
       , Sendable a :<: as
       )
    => (cot (Summed as (Sendable a f)))
    -> m a

  inChanLen
    :: MonadIO m
    => (cit (Summed as (Sendable a f)))
    -> m Int
  outChanLen
    :: MonadIO m
    => (cot (Summed as (Sendable a f)))
    -> m Int
