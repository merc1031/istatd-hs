{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Istatd.Datum.Types
( IstatdDatum (..)
, Counter (..)
, encode
, toPacket
, getKey
, updateKey
, compute
)
where

import            Data.Monoid                         ( (<>) )

import qualified  Data.ByteString                     as BS
import qualified  Data.ByteString.Lazy                as BSL
import qualified  Data.ByteString.Lazy.Char8          as BSLC
import qualified  Data.ByteString.Lazy.Builder        as BSLB
import qualified  Data.Double.Conversion.ByteString   as PrintDouble
import qualified  Data.Time.Clock.POSIX               as POSIX

--data IstatdType =
--    Counter
--  | Gauge
--  deriving (Show, Eq)


data Counter =
    Counter !BSLB.Builder !POSIX.POSIXTime !Double
  | Gauge !BSLB.Builder !POSIX.POSIXTime !Double

data IstatdDatum = IstatdDatum
  { _getKey :: BSLB.Builder
  , _getKeyC :: BSLC.ByteString
  , _updateKey :: BSLB.Builder -> IstatdDatum
  , _compute :: forall m. m Counter
  , _getTime :: POSIX.POSIXTime
  , _getValue :: Double
  }

mkCounter k v =
  let a@(IstatdDatum {..})= IstatdDatum { _getKey = k
                                        , _getKeyC = BSLB.toLazyByteString k
                                        , _updateKey = \nk -> a { _getKey = nk, _getKeyC = BSLB.toLazyByteString nk }
                                        , _compute = return $ Counter _getKey _getTime _getValue
                                        , _getTime = 0
                                        , _getValue = v
                                        }
  in a

getKey :: IstatdDatum -> BSLB.Builder
getKey (IstatdDatum {..}) = _getKey

updateKey :: IstatdDatum -> BSLB.Builder -> IstatdDatum
updateKey (IstatdDatum {..}) key = _updateKey key

compute :: IstatdDatum -> m Counter
compute IstatdDatum {..} = _compute

instance Show Counter where
  show (Counter k t d) =
      "Counter "
    <> (BSLC.unpack $ BSLB.toLazyByteString $ k)
    <> " "
    <> show t
    <> " "
    <> show d
  show (Gauge k t d) =
      "Gauge "
    <> (BSLC.unpack $ BSLB.toLazyByteString $ k)
    <> " "
    <> show t
    <> " "
    <> show d

instance Eq Counter where
  (Counter k t d) == (Counter k' t' d') =
    BSLB.toLazyByteString k == BSLB.toLazyByteString k'
    &&
    t == t'
    &&
    d == d'
  (Gauge k t d) == (Gauge k' t' d') =
    BSLB.toLazyByteString k == BSLB.toLazyByteString k'
    &&
    t == t'
    &&
    d == d'
  _ == _ = False


toPacket
  :: Counter
  -> BS.ByteString
toPacket datum =
  BSL.toStrict . BSLB.toLazyByteString $ encode datum

-- | Serializes an istatd packet as a ByteString.
encode
  :: Counter
  -> BSLB.Builder
encode (Counter name _t value) =
  encode' (BSLB.char7 '*') name value
encode (Gauge name _t value) =
  encode' mempty name value
{-# INLINE encode #-}

encode'
  :: BSLB.Builder
  -> BSLB.Builder
  -> Double
  -> BSLB.Builder
encode' prefix name value =
  prefix <>
  name <>
  BSLB.char7 ' ' <>
  BSLB.byteString (PrintDouble.toShortest value) <>
  BSLB.char7 '\n'
{-# INLINE encode' #-}
