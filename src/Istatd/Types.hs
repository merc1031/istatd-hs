module Istatd.Types
( Rec (..)
, IstatInChan
, IstatOutChan
, FilterFunc
, getKey
, updateKey
)
where

import            Istatd.Chan     ( InChanI
                                  , OutChanI
                                  )

import qualified  Data.Text.Lazy  as TL

type IstatInChan = InChanI Rec
type IstatOutChan = OutChanI Rec

data Rec = Counter TL.Text Int Int
         | Gauge TL.Text Int Double
         deriving (Show)

getKey :: Rec
       -> TL.Text
getKey (Counter k _ _) = k
getKey (Gauge k _ _) = k

updateKey :: TL.Text
          -> Rec
          -> Rec
updateKey k (Counter _ t i) = Counter k t i
updateKey k (Gauge _ t d) = Gauge k t d

type FilterFunc m = (IstatInChan -> m IstatInChan)

