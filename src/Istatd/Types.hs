module Istatd.Types
( IstatdDatum (..)
, IstatdType (..)
, IstatInChan
, IstatOutChan
, FilterFunc
, getKey
, updateKey
, toPacket
)
where

import            Istatd.Chan                         ( InChanI
                                                      , OutChanI
                                                      )
import            Data.Monoid                         ( (<>) )

import qualified  Data.ByteString                     as BS
import qualified  Data.ByteString.Lazy                as BSL
import qualified  Data.ByteString.Lazy.Char8          as BSLC
import qualified  Data.ByteString.Lazy.Builder        as BSLB
import qualified  Data.Double.Conversion.ByteString   as PrintDouble
import qualified  Data.Time.Clock.POSIX               as POSIX

type IstatInChan = InChanI IstatdDatum
type IstatOutChan = OutChanI IstatdDatum

data IstatdType = Counter
                | Gauge
                deriving (Show)

data IstatdDatum = IstatdDatum !IstatdType !BSLB.Builder !POSIX.POSIXTime !Double

instance Show IstatdDatum where
  show (IstatdDatum it k t d) = "IstatdDatum "
                             <> show it
                             <> " "
                             <> (BSLC.unpack $ BSLB.toLazyByteString $ k)
                             <> " "
                             <> show t
                             <> " "
                             <> show d

toPacket :: IstatdDatum
         -> BS.ByteString
toPacket datum = BSL.toStrict . BSLB.toLazyByteString $ encode datum

-- | Serializes an istatd packet as a ByteString.
encode :: IstatdDatum
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

getKey :: IstatdDatum
       -> BSLB.Builder
getKey (IstatdDatum _ k _ _) = k

updateKey :: BSLB.Builder
          -> IstatdDatum
          -> IstatdDatum
updateKey k (IstatdDatum c _k t i) = IstatdDatum c k t i

type FilterFunc m = (IstatInChan -> m IstatInChan)

