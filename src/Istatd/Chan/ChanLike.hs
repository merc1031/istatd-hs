{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Istatd.Chan.ChanLike where

import          Control.Monad.IO.Class      ( MonadIO )
import          Control.Exception           ( Exception )
import          Data.Typeable               ( Typeable )
import Istatd.Simplicity
import Data.Maybe

data ChannelException = ChannelBlockedException
                      deriving (Show, Typeable)

instance Exception ChannelException


-- | `ChanLike` implements an interface for channels that want to talk about
-- in and out sides. They are associated via a cyclical functional dependency.
-- In this way a function using an in(`cit`) or out(`cot`) channel of the correct `a`
-- may bring the type of the associated other end into scope.
-- This interface allows the generalized code to not worry about the type of the
-- specific channel or its underlying concurrency implementation.
class ChanLike (cit :: * -> *) (cot :: * -> *) (as :: [*])
  | cit as -> cot as
  , cot as -> cit as
  where
  newZChan
    :: MonadIO m
    => m (cit (Summed as), cot (Summed as))
  newBChan
    :: MonadIO m
    => Int
    -> m (cit (Summed as), cot (Summed as))

  readRaw
    :: forall m
     . MonadIO m
    => (cot (Summed as))
    -> m (Summed as)

  writeRaw
    :: forall m
     . MonadIO m
    => (cit (Summed as))
    -> Summed as
    -> m ()

  writeChan
    :: forall a m
     . ( MonadIO m
       , a :<: as
       )
    => (cit (Summed as))
    -> a
    -> m ()

  readChan
    :: forall a m
     . ( MonadIO m
       , a :<: as
       )
    => (cot (Summed as))
    -> m a
  readChan c = fromJust <$> (readChanM c)

  readChanM
    :: forall a m
     . ( MonadIO m
       , a :<: as
       )
    => (cot (Summed as))
    -> m (Maybe a)

  inChanLen
    :: MonadIO m
    => (cit (Summed as ))
    -> m Int
  outChanLen
    :: MonadIO m
    => (cot (Summed as ))
    -> m Int
