{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
module Istatd.TypeSet
( (:||)
, (:<)
, Nat (..)
, InSet
, InList
, SetToCompound
, ToCompound
, SetDelete
, DeleteInList
, SetInsert
, ListInsert
, UniqueInsert
, Set
, FirstIdxList
)
where

import CompoundTypes.Lazy
import Prelude hiding (elem)

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

-- | Type family for finding the compound sum type for a givent type level set
type family SetToCompound (s :: *) (a :: *) :: * where
  SetToCompound (Set s) a = ToCompound s a

-- | Type for finding the compound sum type for a givent type level list
type ToCompound (rs :: [*]) (a :: *) =
  ToCompound' (InList rs a ) (Partition rs a) a

-- | Inner Type family for finding the compound sum type for a givent type level list
type family ToCompound' (b :: Bool) (ps :: ([*], [*])) (a :: *) :: * where
  ToCompound' 'True '( '[], '[] ) a = a
  ToCompound' 'True '( '[], rs )  a = a + (ToCompoundRest rs)
  ToCompound' 'True '( ls, '[] )  a = (ToCompoundRest ls) + a
  ToCompound' 'True '( ls, rs )   a = (ToCompoundRest ls) + a + (ToCompoundRest rs)

-- | Inner Type family for finding the remaining compound sum type for a givent type level list
type family ToCompoundRest (rs :: [*]) :: * where
  ToCompoundRest (x ': '[]) = x
  ToCompoundRest (x ': xs)  = x + (ToCompoundRest xs)
