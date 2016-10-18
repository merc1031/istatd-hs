{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Istatd.ChanData
( InData (..)
, OutData (..)
, OutConstraint
)
where

import GHC.Exts (Constraint)
import qualified  Data.ByteString.Lazy.Builder        as BSLB

type family OutConstraint a b (m :: * -> *) :: Constraint

class (OutData b) => InData a b | a -> b where
  type OutState a b :: *
  getKey :: a -> BSLB.Builder
  updateKey :: a -> BSLB.Builder -> a
  toOutData :: OutConstraint a b m => a -> OutState a b -> m b

class OutData a where
  getValue :: a -> Double
