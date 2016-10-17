module Istatd.Types
( IstatdDatum (..)
, FilterFunc
, SinkState
, mkSinkState
, getKey
, updateKey
, toPacket
)
where

import            Control.Concurrent.STM
import            Control.Monad.IO.Class              ( MonadIO
                                                      , liftIO
                                                      )
import            Data.Monoid                         ( (<>) )

import qualified  Data.ByteString                     as BS
import qualified  Data.ByteString.Lazy                as BSL
import qualified  Data.ByteString.Lazy.Char8          as BSLC
import qualified  Data.ByteString.Lazy.Builder        as BSLB
import qualified  Data.Double.Conversion.ByteString   as PrintDouble
import qualified  Data.HashMap.Strict                 as HM
import qualified  Data.Time.Clock.POSIX               as POSIX


data IstatdDatum = Counter !BSLB.Builder !POSIX.POSIXTime !Double
                 | Gauge !BSLB.Builder !POSIX.POSIXTime !Double
                 | Difference !BSLB.Builder !POSIX.POSIXTime !Double

type FilterFunc c m = (c -> m c)

instance Show IstatdDatum where
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
  show (Difference k t d) =
      "Difference "
    <> (BSLC.unpack $ BSLB.toLazyByteString $ k)
    <> " "
    <> show t
    <> " "
    <> show d

instance Eq IstatdDatum where
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
  (Difference k t d) == (Difference k' t' d') =
    BSLB.toLazyByteString k == BSLB.toLazyByteString k'
    &&
    t == t'
    &&
    d == d'
  _ == _ = False

-- Should allow sink to compute Percentil and Difference counters
newtype SinkState = SinkState (TVar (HM.HashMap BSLC.ByteString Double))

mkSinkState :: MonadIO m
            => m SinkState
mkSinkState = SinkState <$> (liftIO $ newTVarIO HM.empty)

toPacket :: MonadIO m
         => SinkState
         -> IstatdDatum
         -> m BS.ByteString
toPacket state datum =
  BSL.toStrict . BSLB.toLazyByteString <$> toPacket' state datum

toPacket' :: MonadIO m
          => SinkState
          -> IstatdDatum
          -> m BSLB.Builder
toPacket' _ (Counter k _t v) =
  return $ encode'' '*' k v
toPacket' _ (Gauge k _t v) =
  return $ encode' k v
toPacket' (SinkState stateV) (Difference k _t v) = do
  v' <- liftIO $ atomically $ do
      oldH <- readTVar stateV
      let k' = BSLB.toLazyByteString k
          diff = case HM.lookup k' oldH of
            Just d -> v - d
            Nothing -> 0
      modifyTVar' stateV (\h -> HM.insert k' v h)
      return diff
  return $ encode' k v'

encode' :: BSLB.Builder
        -> Double
        -> BSLB.Builder
encode' name value =
  name <>
  BSLB.char7 ' ' <>
  BSLB.byteString (PrintDouble.toShortest value) <>
  BSLB.char7 '\n'
{-# INLINE encode' #-}

encode'' :: Char
         -> BSLB.Builder
         -> Double
         -> BSLB.Builder
encode'' prefix name value = (BSLB.char7 prefix) <> encode' name value
{-# INLINE encode'' #-}

--toPacket :: IstatdDatum
--         -> BS.ByteString
--toPacket datum =
--  BSL.toStrict . BSLB.toLazyByteString $ encode datum
--
---- | Serializes an istatd packet as a ByteString.
--encode :: IstatdDatum
--       -> BSLB.Builder
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

getKey :: IstatdDatum
       -> BSLB.Builder
getKey (Counter k _ _) = k
getKey (Gauge k _ _) = k
getKey (Difference k _ _) = k

updateKey :: BSLB.Builder
          -> IstatdDatum
          -> IstatdDatum
updateKey k (Counter _k t i) =
  Counter k t i
updateKey k (Gauge _k t i) =
  Gauge k t i
updateKey k (Difference _k t i) =
  Difference k t i
