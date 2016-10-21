{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Istatd.Types
( IstatdDatum (..)
, IstatdType (..)
, HasKey (..)
, FilterFunc
, FilterFuncT
, toPacket
)
where

import            Data.Monoid                         ( (<>) )

import qualified  Data.ByteString                     as BS
import qualified  Data.ByteString.Lazy                as BSL
import qualified  Data.ByteString.Lazy.Char8          as BSLC
import qualified  Data.ByteString.Lazy.Builder        as BSLB
import qualified  Data.Double.Conversion.ByteString   as PrintDouble
import qualified  Data.Time.Clock.POSIX               as POSIX

import Istatd.TypeSet (Cmp)

data IstatdType =
    Counter
  | Gauge
  deriving (Show, Eq)

type instance Cmp IstatdDatum IstatdDatum = 'EQ

data IstatdDatum =
  IstatdDatum !IstatdType !BSLB.Builder !POSIX.POSIXTime !Double

type FilterFunc c m = (c -> m c)
type FilterFuncT ci co m = (ci -> m co)

class HasKey a where
  getKey :: a -> BSLB.Builder
  updateKey :: a -> BSLB.Builder -> a


instance HasKey IstatdDatum where
  getKey (IstatdDatum _ k _ _) = k

  updateKey (IstatdDatum c _k t i) k =
    IstatdDatum c k t i

instance Show IstatdDatum where
  show (IstatdDatum it k t d) =
      "IstatdDatum "
    <> show it
    <> " "
    <> (BSLC.unpack $ BSLB.toLazyByteString $ k)
    <> " "
    <> show t
    <> " "
    <> show d

instance Eq IstatdDatum where
  (IstatdDatum it k t d) == (IstatdDatum it' k' t' d') =
    it == it'
    &&
    BSLB.toLazyByteString k == BSLB.toLazyByteString k'
    &&
    t == t'
    &&
    d == d'


toPacket
  :: IstatdDatum
  -> BS.ByteString
toPacket datum =
  BSL.toStrict . BSLB.toLazyByteString $ encode datum

-- | Serializes an istatd packet as a ByteString.
encode
  :: IstatdDatum
  -> BSLB.Builder
encode (IstatdDatum ptype name _t value) =
  prefix <>
  name <>
  BSLB.char7 ' ' <>
  BSLB.byteString (PrintDouble.toShortest value) <>
  BSLB.char7 '\n'
    where
      prefix = case ptype of
        Counter -> BSLB.char7 '*'
        Gauge -> mempty
{-# INLINE encode #-}
