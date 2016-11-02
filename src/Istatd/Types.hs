module Istatd.Types
( HasKey (..)
, FilterFunc
, FilterFuncT
--, toPacket
)
where


import qualified  Data.ByteString.Lazy.Builder        as BSLB


type FilterFunc c m = (c -> m c)
type FilterFuncT ci co m = (ci -> m co)

class HasKey a where
  getKey :: a -> BSLB.Builder
  updateKey :: a -> BSLB.Builder -> a



--toPacket
--  :: IstatdDatum
--  -> BS.ByteString
--toPacket datum =
--  BSL.toStrict . BSLB.toLazyByteString $ encode datum
--
---- | Serializes an istatd packet as a ByteString.
--encode
--  :: IstatdDatum
--  -> BSLB.Builder
--encode (IstatdDatum ptype name _t value) =
--  prefix <>
--  name <>
--  BSLB.char7 ' ' <>
--  BSLB.byteString (PrintDouble.toShortest value) <>
--  BSLB.char7 '\n'
--    where
--      prefix = case ptype of
--        Counter -> BSLB.char7 '*'
--        Gauge -> mempty
--{-# INLINE encode #-}
