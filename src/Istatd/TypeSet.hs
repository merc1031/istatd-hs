{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Istatd.TypeSet
( (:||)
, (:<)
, Nat (..)
, InSet
, InList
, SetToCompound
, ToCompound
, SetToCompoundAdd
, ToCompoundAdd
, SetDelete
, DeleteInList
, SetInsert
, ListInsert
, UniqueInsert
, Set
, FirstIdxList
, SetFromCompound
, FromCompound
, gconstrn
, gconstr
, ShouldHandle (..)
, uhhh
, shower
)
where

import CompoundTypes.Lazy
import Data.Data
import Control.Monad (msum)
import Data.Maybe
import Prelude hiding (elem)
import Debug.Trace

--import Data.Type.Equality

-- | Simple Type Level Set, wrapper around Type level list
data Set :: [*] -> * where
--  SHead :: a -> Set '[a]
--  SNext :: a -> Set r -> Set (ListInsert (InList r a) r a)

-- | Type level `||`
type family (a :: Bool) :|| (b :: Bool) where
  'True :|| _     = 'True
  _     :|| 'True = 'True
  _     :|| _     = 'False


-- | Type level natural numbers
data Nat :: * where
    Zero :: Nat
    Succ :: Nat -> Nat

--data SNat (n :: Nat) where
--    SZero :: SNat 'Zero
--    SSucc :: SNat n -> SNat ('Succ n)

-- | Type level `<`
type family (m :: Nat) :< (n :: Nat) :: Bool where
    m         :< 'Zero     = 'False
    'Zero     :< ('Succ n) = 'True
    ('Succ m) :< ('Succ n) = m :< n

-- | Type family for inserting into a type level set
type family SetInsert (s :: *) (a :: *) :: * where
  SetInsert (Set rs) a = Set (ListInsert (InList rs a) rs a)

infixr 0 +!

type a +! rs = UniqueInsert rs a

-- | Type family for inserting non duplicates into type level list
type UniqueInsert (rs :: [*]) (a :: *) =
  ListInsert (InList rs a) rs a

-- | Type family for conditional inserting into type level list
type family ListInsert (b :: Bool) (rs :: [*]) (a :: *) :: [*] where
  ListInsert 'False r a = a ': r
  ListInsert _      r _ = r

-- | Type family for list membership
type family InSet (s :: *) (a :: *) :: Bool where
  InSet (Set rs) a = InList rs a

-- | Type family for list membership
type family InList (rs :: [*]) (a :: *) :: Bool where
  InList '[]       x = 'False
  InList (x ': ys) x = 'True
  InList (y ': ys) x = 'False :|| (InList ys x)

-- | Type family for finding index in type level list
type family FirstIdxList (rs :: [*]) (a :: *) :: Nat where
  FirstIdxList (x ': ys) x = 'Zero
  FirstIdxList (y ': ys) x = 'Succ (FirstIdxList ys x)

-- | Type family for deleting element from type level set
type family SetDelete (s :: *) (a :: *) :: * where
  SetDelete (Set r) a = Set (DeleteInList r a)

-- | Type family for deleting element from type level list
type family DeleteInList (rs :: [*]) (a :: *) :: [*] where
  DeleteInList '[]       _ = '[]
  DeleteInList (a ': rs) a = rs
  DeleteInList (b ': rs) a = b ': (DeleteInList rs a)

-- | Type family for getting elements to the left of the element
-- in a type level list
type family PartitionLeft (rs :: [*]) (a :: *) :: [*] where
  PartitionLeft (a ': rs) a = '[]
  PartitionLeft (b ': rs) a = b ': (PartitionLeft rs a)

-- | Type family for getting elements to the right of the element
-- in a type level list
type family PartitionRight (rs :: [*]) (a :: *) :: [*] where
  PartitionRight (a ': rs) a = rs
  PartitionRight (b ': rs) a = PartitionRight rs a

-- | Type family for partitioning a type level list around a pivot element
type family Partition (rs :: [*]) (a :: *) :: ([*], [*]) where
  Partition rs a = '(PartitionLeft rs a, PartitionRight rs a)
-- 
-- | Type family for finding the compound sum type for a givent type level set
type family SetToCompoundAdd (s :: *) (a :: *) :: * where
  SetToCompoundAdd (Set rs) a = ToCompoundAdd rs a

-- | Type for finding the compound sum type for a givent type level list
type ToCompoundAdd (rs :: [*]) (a :: *) =
  ToCompoundAdd' (InList rs a ) (Partition rs a) a

-- | Inner Type family for finding the compound sum type for a givent type level list
type family ToCompoundAdd' (b :: Bool) (ps :: ([*], [*])) (a :: *) :: * where
  ToCompoundAdd' 'True '( '[], '[] ) a = a
  ToCompoundAdd' 'True '( '[], rs )  a = a + (ToCompound rs)
  ToCompoundAdd' 'True '( ls, '[] )  a = (ToCompound ls) + a
  ToCompoundAdd' 'True '( ls, rs )   a = (ToCompound ls) + a + (ToCompound rs)

-- | Type family for finding the compound sum type for a givent type level set
type family SetToCompound (s :: *) :: * where
  SetToCompound (Set rs) = ToCompound rs

-- | Inner Type family for finding the remaining compound sum type for a givent type level list
type family ToCompound (rs :: [*]) :: * where
  ToCompound (x ': '[]) = x
  ToCompound (x ': xs)  = x + (ToCompound xs)

type family SetFromCompound (a :: *) :: * where
  SetFromCompound a = Set (FromCompound a)

type family FromCompound (a :: *) :: [*] where
  FromCompound (Sum4 a b c d) = a +! b +! c +! d +! '[]
  FromCompound (Sum3 a b c) = a +! b +! c +! '[]
  FromCompound (Sum2 a b) = a +! b +! '[]
  FromCompound a = a +! '[]

deriving instance (Data a, Data b) => Data (Sum2 a b)
deriving instance (Typeable a, Typeable b) => Typeable (Sum2 a b)

deriving instance (Data a, Data b, Data c) => Data (Sum3 a b c)
deriving instance (Typeable a, Typeable b, Typeable c) => Typeable (Sum3 a b c)

deriving instance (Data a, Data b, Data c, Data d) => Data (Sum4 a b c d)
deriving instance (Typeable a, Typeable b, Typeable c, Typeable d) => Typeable (Sum4 a b c d)

deriving instance (Show a, Show b) => Show (Sum2 a b)

deriving instance (Show a, Show b, Show c) => Show (Sum3 a b c)

deriving instance (Show a, Show b, Show c, Show d) => Show (Sum4 a b c d)

gconstrn :: (Typeable a, Data t) => Constr -> a -> Maybe t
gconstrn constr arg = gunfold addArg Just constr
    where
        addArg :: Data b => Maybe (b -> r) -> Maybe r
        addArg Nothing = Nothing
        addArg (Just f) =
            case cast arg of
                Just v -> Just (f v)
                Nothing -> Nothing

gconstr :: (Typeable a, Data t) => a -> Maybe t
gconstr arg = result
    where
        result = msum $ map (gunfold (<*> cast arg) Just) (dataTypeConstrs dt)
        dt = dataTypeOf (fromJust result)

data ShouldHandle a b = ShouldHandle a
                      | ShouldntHandle b
                      deriving (Show, Data, Typeable)

shower :: Show a => Show b => ShouldHandle a b -> IO ()
shower (ShouldHandle a) = putStrLn $ "Handling " ++ show a
shower (ShouldntHandle a) = putStrLn $ "Not Handling " ++ show a

uhhh :: forall m a compoundUs compoundRest
      . ( Monad m
        , a ~ (compoundUs - compoundRest)
        , Data compoundUs
        , Data compoundRest
        , Data a
        , Show a
        , Show compoundRest
        )
     => Proxy compoundRest
     -> compoundUs
     -> (ShouldHandle a compoundRest -> m ())
     -> m ()
uhhh _ dat a =
  let wtf = head $ gmapQ (\d -> cast d) (dat :: compoundUs) :: Maybe a
  in case traceShowId wtf of
       Just v -> case traceShowId $ gconstr v :: Maybe (ShouldHandle a compoundRest) of
                   Just v' -> a v'
                   Nothing -> return ()
       Nothing -> return () -- is a
--  let wtf = gconstr (fromJust $ head $ gmapQ (\d -> cast d) (dat :: compoundUs) :: a) :: (Maybe (ShouldHandle a compoundRest))
--  in case traceShowId wtf of
--       Just v -> a v
--       Nothing -> return () -- is a

-- (fromConstr $ head $ gmapQ (\d -> toConstr d) (Sum3_2 5 :: Sum3 Char Int String) :: Int)
-- 5
-- constrIndex $ toConstr $ (Sum3_2 5 :: Sum3 Char Int String)
-- 2
-- gconstr (fromConstr $ head $ gmapQ (\d -> toConstr d) (Sum3_2 5 :: Sum3 Char Int String) :: Int) :: (Maybe (Sum2 Char Int))
-- Just (Sum2_2 5)
----gconv :: inT
----      -> outT
--gconv :: (Data t1) => t1 -> Maybe a
--gconv v =
--  fromConstrB (fromConstr (toConstr (v :: t1))) (toConstr (Just _t :: Maybe a)) :: Maybe a
