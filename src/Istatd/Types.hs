{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Istatd.Types
( IstatdDatum (..)
, IstatdType (..)
, FilterFunc
, FilterFuncT
, AnyFilterFunc (..)
, getKey
, updateKey
, toPacket
)
where

import            Data.Monoid                         ( (<>) )
import            Istatd.ChanData                     ( InData (..)
                                                      , OutData (..)
                                                      , OutConstraint
                                                      )

import qualified  Data.ByteString                     as BS
import qualified  Data.ByteString.Lazy                as BSL
import qualified  Data.ByteString.Lazy.Char8          as BSLC
import qualified  Data.ByteString.Lazy.Builder        as BSLB
import qualified  Data.Double.Conversion.ByteString   as PrintDouble
import qualified  Data.Time.Clock.POSIX               as POSIX

type instance OutConstraint IstatdDatum IstatdDatum m = Monad m
instance InData IstatdDatum IstatdDatum where
  type OutState IstatdDatum IstatdDatum = ()
  getKey (IstatdDatum _ k _ _) = k
  updateKey (IstatdDatum it _k t d) k =
    IstatdDatum it k t d
  toOutData a _ = return a

instance OutData IstatdDatum where
  getValue (IstatdDatum _ _ _ v) = v

data IstatdType =
    Counter
  | Gauge
  deriving (Show, Eq)


data IstatdDatum =
  IstatdDatum !IstatdType !BSLB.Builder !POSIX.POSIXTime !Double

type FilterFunc c m = FilterFuncT c c m
type FilterFuncT ci co m = (ci -> m co)

data AnyFilterFunc = forall ci co m. (InData ci co, OutData co) => AnyFilterFunc (FilterFuncT ci co m)

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

--getKey :: IstatdDatum
--       -> BSLB.Builder
--getKey (IstatdDatum _ k _ _) = k
--
--updateKey :: BSLB.Builder
--          -> IstatdDatum
--          -> IstatdDatum
--updateKey k (IstatdDatum c _k t i) =
--  IstatdDatum c k t i
