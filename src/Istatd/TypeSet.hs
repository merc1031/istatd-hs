{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
module Istatd.TypeSet
( insert
, InSet
, Delete
, singleton
, (:||)
)
where

import Prelude hiding (elem)

--import Data.Type.Equality

data Set :: [*] -> * where
  SHead :: a -> Set '[a]
  SNext :: a -> Set r -> Set (ListInsert (InList a r) a r)

type family a :|| b where
  'True :|| _ = 'True
  _ :|| 'True = 'True
  _ :|| _ = 'False


data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat

--data SNat (n :: Nat) where
--    SZero :: SNat 'Zero
--    SSucc :: SNat n -> SNat ('Succ n)

type family (m :: Nat) :< (n :: Nat) :: Bool where
    m :< 'Zero = 'False
    'Zero :< ('Succ n) = 'True
    ('Succ m) :< ('Succ n) = m :< n

type family SetInsert a r where
  SetInsert a r = (ListInsert (InList a r) a r)

type family ListInsert n a r where
  ListInsert 'False a r = a ': r
  ListInsert _ _ r = r




type family MayAppend b r a where
  MayAppend 'True r a = r
  MayAppend 'False r a = a ': r

type family InList a r where
  InList x '[] = 'False
  InList x (x ': ys) = 'True
  InList x (y ': ys) = 'False :|| (InList x ys)


type family FirstIdxList a r where
  FirstIdxList x (x ': ys) = 'Zero
  FirstIdxList x (y ': ys) = 'Succ (FirstIdxList x ys)

type family InSet a s where
  InSet a (Set r) = InList a r

type family DeleteInList a rs where
  DeleteInList _ '[] = '[]
  DeleteInList a (a ': rs) = rs
  DeleteInList a (b ': rs) = b ': (DeleteInList a rs)

type family Delete a s where
  Delete a (Set r) = DeleteInList a r

insert :: (
          r' ~ SetInsert a r
          )
       => a
       -> Set r
       -> Set r'
insert elem set = SNext elem set

delete :: (
          r' ~ SetInsert a r
          )
       => a
       -> Set r
       -> Set r'
delete elem set = SNext elem set

singleton :: a -> Set '[a]
singleton elem = SHead elem
