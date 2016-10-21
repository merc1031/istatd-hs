{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Istatd.TypeSet
( (:||)
, (:<)
, Nat (..)
, SNat (..)
, SNatRep (..)
, InSet
, InList
, SetLength
, ListLength
, SetToCompound
, ToCompound
, SetToCompoundAdd
, ToCompoundAdd
, SetDelete
, ListDelete
, SetInsert
, ListInsert
, UniqueInsert
, (:+!)
, Set
, FirstIdxList
, SetFromCompound
, FromCompound
, IsSubSet
, IsSubList
, SetToVariant
, SetFromVariant
-----------------------
, gconstrn
, gconstr
, ShouldHandle (..)
, inList
, isSubSet
, isSubList
, isSubSetC
, isSubListC
-----------------------
, Variant
, mkVariantV
, mkVariant
, splitVariant
, VariantCompatibility
, VariantDiff
, InVariant
, Implicit (..)
, Elem (..)
, IElem
, Subset (..)
, ISubset
, In
, Cmp
, Sort
, AsSet
, MinVariant
)
where

import CompoundTypes.Lazy
--import Data.HList.Variant
import Data.Data
import Control.Monad (msum)
import Data.Maybe
import Prelude hiding (elem)
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.Type.Bool
import Data.Type.Equality

class Implicit p where
  implicitly :: p

-- | An inductive list membership proposition.
data Elem :: k -> [k] -> * where
  Here  :: Elem x (x ': xs)
  There :: Elem x xs -> Elem x (y ': xs)

-- | A constraint for implicit resolution of list membership proofs.
type IElem x xs = Implicit (Elem x xs)

-- | An inductive list subset relation.
data Subset :: [k] -> [k] -> * where
  SubsetNil  :: Subset '[] xs
  SubsetCons :: Elem x ys
             -> Subset xs ys
             -> Subset (x ': xs) ys

-- | A constraint for implicit resolution of list subset proofs.
type ISubset xs ys = Implicit (Subset xs ys)

instance Implicit (Elem x (x ': xs)) where
  implicitly = Here
instance Implicit (Elem x xs) => Implicit (Elem x (y ': xs)) where
  implicitly = There implicitly

instance Implicit (Subset '[] xs) where
  implicitly = SubsetNil
instance (IElem x ys, ISubset xs ys) => Implicit (Subset (x ': xs) ys) where
  implicitly = SubsetCons implicitly implicitly

type MinVariant xs = Variant (AsSet xs)
--import Data.Type.Equality
--
--
data Variant (vs :: [*]) = Variant !Int Any
type role Variant representational

--class MkVariant x v vs | x vs -> v where
--  mkVariant :: v -> proxy vs -> Variant vs
--
--extendVariant :: Variant l -> Variant (e ': l)
--extendVariant (Variant m e) = Variant (m+1) e
--
mkVariant :: forall v n vs vss
           . ( --vss ~ (UniqueInsert vs v)
               n ~ (FirstIdxList vss v)
             , vss ~ (AsSet (v ': vs))
             , SNatRep n
             , IElem v vss
             , ISubset vs vss
             )
          => Proxy vs
          -> v
          -> Variant vss
mkVariant _ v = Variant (sNatToInt idx) (unsafeCoerce v)
  where
    idx :: SNat n
    idx = getSNat

mkVariantV :: forall v n vs vss
            . ( vss ~ (AsSet (v ': vs))--vss ~ (UniqueInsert vs v)
              , n ~ (FirstIdxList vss v)
              , SNatRep n
              )
           => Proxy (Variant vs)
           -> v
           -> Variant vss
mkVariantV _ v = Variant (sNatToInt idx) (unsafeCoerce v)
  where
    idx :: SNat n
    idx = getSNat

splitVariant :: Variant (x ': xs) -> Either x (Variant xs)
splitVariant (Variant 0 x) = Left (unsafeCoerce x)
splitVariant (Variant n x) = Right (Variant (n-1) x)

instance (ShowVariant vs) => Show (Variant vs) where
  showsPrec _ v = ("V{" ++) . showVariant v . ('}':)

class ShowVariant vs where
  showVariant :: Variant vs -> ShowS

instance (Show v, ShowVariant (w ': ws)) => ShowVariant (v ': w ': ws) where
  showVariant vs = case splitVariant vs of
                     Left v -> \rest -> show v ++ rest
                     Right wws -> showVariant wws

instance (Show v) => ShowVariant ('[v]) where
  showVariant vs = case splitVariant vs of
                     Left v -> \rest -> show v ++ rest
                     Right _ -> error "Invalid"



type family VariantCompatibility (v :: *) (vs :: *) :: Bool where
  VariantCompatibility (Variant ss) (Variant vss) = IsSubList ss vss

type family VariantDiff (v :: *) (vs :: *) = (r :: *) where
  VariantDiff (Variant ss) (Variant vss) = Variant (ListDiff ss vss)
-- 
-- | Type family for list membership
type family InVariant (s :: *) (a :: *) :: Bool where
  InVariant (Variant rs) a = InList rs a

type family ListDiff (v :: [*]) (vs :: [*]) = (r :: [*]) where
  ListDiff '[]          '[]           = '[]
  ListDiff ss           '[]           = ss
  ListDiff ss           (v ': vs)     = ListDiff (ListDelete ss v) vs

-- | Simple Type Level Set, wrapper around Type level list
data Set :: [*] -> * where
--  SHead :: a -> Set '[a]
--  SNext :: a -> Set r -> Set (ListInsert (InList r a) r a)

-- | Type level `||`
type family (a :: Bool) :|| (b :: Bool) where
  'True :|| _     = 'True
  _     :|| 'True = 'True
  _     :|| _     = 'False
--
-- | Type level `&&`
type family (a :: Bool) :&& (b :: Bool) where
  'True :&& 'True = 'True
  _     :&& _     = 'False



-- | Type level natural numbers
data Nat :: * where
  Zero :: Nat
  Succ :: Nat -> Nat

data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

sNatToInt :: SNat n -> Int
sNatToInt SZero = 0
sNatToInt (SSucc n) = 1 + (sNatToInt n)

class SNatRep n where
  getSNat :: SNat n

instance SNatRep 'Zero where
  getSNat = SZero

instance SNatRep n => SNatRep ('Succ n) where
  getSNat = SSucc getSNat

type family SetLength (s :: *) :: Nat where
  SetLength (Set rs) = ListLength rs

type family ListLength (rs :: [*]) :: Nat where
  ListLength '[]       = 'Zero
  ListLength (_ ': ys) = 'Succ (ListLength ys)

-- | Type level `<`
type family (m :: Nat) :< (n :: Nat) :: Bool where
  m         :< 'Zero     = 'False
  'Zero     :< ('Succ n) = 'True
  ('Succ m) :< ('Succ n) = m :< n

-- | Type family for inserting into a type level set
type family SetInsert (s :: k) (a :: k) :: k where
  SetInsert (Set rs) a = Set (ListInsert (InList rs a) rs a)

infixr 0 :+!

type In rs a rss = (rss ~ UniqueInsert rs a, 'True ~ InList rss a)

type (a :: k) :+! (rs :: [k]) =
  UniqueInsert rs a

-- | Type family for inserting non duplicates into type level list
type UniqueInsert (rs :: [k]) (a :: k) =
  ListInsert (InList rs a) rs a

-- | Type family for conditional inserting into type level list
type family ListInsert (b :: Bool) (rs :: [k]) (a :: k) = (r :: [k]) where
  ListInsert 'False r a = a ': r
  ListInsert 'True  r _ = r

-- | Type family for list membership
type family IsSubSet (s :: *) (sub :: *) :: Bool where
  IsSubSet (Set rs) (Set subrs) = IsSubList rs subrs

type family IsSubList (rs :: [*]) (subrs :: [*]) :: Bool where
  IsSubList rs '[] = 'True
  IsSubList rs (sr ': srs) = (InList rs sr) :&& (IsSubList rs srs)


-- | Type family for list membership
type family InSet (s :: *) (a :: *) :: Bool where
  InSet (Set rs) a = InList rs a

-- | Type family for list membership
type family InList (rs :: [k]) (a :: k) = (r :: Bool) where
  InList '[] x = 'False
  InList (x ': ys) x = 'True
  InList (y ': ys) x = InList ys x

--type family NotInList (a :: *) (rs :: [*]) :: Bool where
--  NotInList x (x ': ys) = 'False
--  NotInList x (y ': ys) = NotInList x ys
--  NotInList x '[] = 'True

--type family InList2 (a :: *) (rs :: [*]) :: Bool where
--  InList2 x '[] = 'False
--  InList2 x (x ': ys) = 'True
--  InList2 x (y ': ys) = InList2 x ys


-- | Type family for finding index in type level list
type family FirstIdxList (rs :: [*]) (a :: *) :: Nat where
  FirstIdxList (x ': ys) x = 'Zero
  FirstIdxList (y ': ys) x = 'Succ (FirstIdxList ys x)

-- | Type family for deleting element from type level set
type family SetDelete (s :: *) (a :: *) :: * where
  SetDelete (Set rs) a = Set (ListDelete rs a)

-- | Type family for deleting element from type level list
type family ListDelete (rs :: [*]) (a :: *) :: [*] where
  ListDelete '[]       _ = '[]
  ListDelete (a ': rs) a = rs
  ListDelete (b ': rs) a = b ': (ListDelete rs a)

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

type family FromCompound (a :: k) :: [k] where
  FromCompound (Sum4 a b c d) = a :+! b :+! c :+! d :+! '[]
  FromCompound (Sum3 a b c) = a :+! b :+! c :+! '[]
  FromCompound (Sum2 a b) = a :+! b :+! '[]
  FromCompound a = a :+! '[]


-- | Type family for finding the compound sum type for a givent type level set
type family SetToVariant (s :: *) :: * where
  SetToVariant (Set rs) = Variant rs

---- | Inner Type family for finding the remaining compound sum type for a givent type level list
--type family ToVariant (rs :: [*]) :: * where
--  ToVariant (x ': '[]) = Variant '[x]
--  ToVariant (x ': xs)  = x + (ToVariant xs)

type family SetFromVariant (a :: *) :: * where
  SetFromVariant (Variant rs) = Set rs

--type family FromVariant (a :: k) :: [k] where
--  FromVariant (Sum4 a b c d) = a :+! b :+! c :+! d :+! '[]
--  FromVariant (Sum3 a b c) = a :+! b :+! c :+! '[]
--  FromVariant (Sum2 a b) = a :+! b :+! '[]
--  FromVariant a = a :+! '[]


--class Conversions rs srs where
--  conv :: Proxy rs -> rs -> srs
----
----class Tester rs srs where
----
----instance (InList2 a '[b,a] ~ 'True) => Tester (Sum2 a b) a where
--
--instance ('True ~ IsSubSet (SetFromCompound (Sum2 a b)) (SetFromCompound a), Data a, Data b) => Conversions (Sum2 a b) (a) where
--  conv _ dat =
--    let wtf = fromJust $ head $ gmapQ (\d -> cast d) (dat :: Sum2 a b) :: a
--    in fromJust $ gconstr wtf :: a
--
--instance ('True ~ IsSubSet (SetFromCompound (Sum3 a b c)) (SetFromCompound (Sum2 a b)), Data a, Data b, Data c) => Conversions (Sum3 a b c) (Sum2 a b) where
--  conv _ dat =
--    let wtf = fromJust $ head $ gmapQ (\d -> cast d) (dat :: Sum3 a b) :: a
--    in fromJust $ gconstr wtf :: Sum2 a b

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

--shower :: Show a => Show b => ShouldHandle a b -> IO ()
--shower (ShouldHandle a) = putStrLn $ "Handling " ++ show a
--shower (ShouldntHandle a) = putStrLn $ "Not Handling " ++ show a
--
--uhhh = undefined
--uhhh :: forall m a compoundUs compoundRest
--      . ( Monad m
--        , a ~ (compoundUs - compoundRest)
--        , Data compoundUs
--        , Data compoundRest
--        , Data a
--        , Show a
--        , Show compoundRest
----        , Conversions compoundUs compoundRest
--        )
--     => Proxy compoundRest
--     -> compoundUs
--     -> (ShouldHandle a compoundRest -> m ())
--     -> m ()
--uhhh _ dat a =
--  let wtf = head $ gmapQ (\d -> cast d) (dat :: compoundUs) :: Maybe a
--  in case traceShowId wtf of
--       Just v -> case traceShowId $ gconstr v :: Maybe (ShouldHandle a compoundRest) of
--                   Just v' -> a v'
--                   Nothing -> return ()
--       Nothing -> a $ ShouldntHandle $ (fromJust $ gconstr v :: compoundRest) --conv (Proxy :: Proxy compoundUs) dat

--forT :: ( cs ~ FromCompound compoundRest
--        )
--     => Proxy compoundRest
--     -> compoundUs
--     -> compoundRest
--forT _ dat = 



--  let wtf = head $ gmapQ (\d -> cast d) (dat :: compoundUs) :: Maybe a
--  in case traceShowId wtf of
--       Just v -> case traceShowId $ gconstr v :: Maybe (ShouldHandle a compoundRest) of
--                   Just v' -> a v'
--                   Nothing -> return ()
--       Nothing -> let wtf' = head $ gmapQ (\d -> cast d) (dat :: compoundUs) :: Maybe a
--                  in case traceShowId $ wtf' of
--                       Just v' -> a v
--                       Nothing -> return ()

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
inList :: ( InList x y ~ 'True
          )
       => Proxy x
       -> Proxy y
       -> ()
inList _ _ = ()

isSubSetC :: ( IsSubSet (SetFromCompound rs) (SetFromCompound srs) ~ 'True
            )
         => Proxy rs
         -> Proxy srs
         -> ()
isSubSetC _ _ = ()

isSubSet :: ( IsSubSet (rs) (srs) ~ 'True
            )
         => Proxy rs
         -> Proxy srs
         -> ()
isSubSet _ _ = ()

isSubListC :: ( IsSubList (FromCompound rs) (FromCompound srs) ~ 'True
            )
         => Proxy rs
         -> Proxy srs
         -> ()
isSubListC _ _ = ()

isSubList :: ( IsSubList (rs) (srs) ~ 'True
            )
         => Proxy rs
         -> Proxy srs
         -> ()
isSubList _ _ = ()

{-| Remove duplicates from a sorted list -}
type family Nub (t :: [k]) :: [k] where
    Nub '[]           = '[]
    Nub '[e]          = '[e]
    Nub (e ': e ': s) = Nub (e ': s)
    Nub (e ': f ': s) = e ': Nub (f ': s)

type AsSet s = Nub (Sort s)

{-| List append (essentially set disjoint union) -}
type family (:++) (x :: [k]) (y :: [k]) :: [k] where
            '[]       :++ xs = xs
            (x ': xs) :++ ys = x ': (xs :++ ys)

{-| Type-level quick sort for normalising the representation of sets -}
type family Sort (xs :: [k]) :: [k] where
            Sort '[]       = '[]
            Sort (x ': xs) = ((Sort (Filter 'FMin x xs)) :++ '[x]) :++ (Sort (Filter 'FMax x xs))

data Flag = FMin | FMax

type family Filter (f :: Flag) (p :: k) (xs :: [k]) :: [k] where
            Filter f p '[]       = '[]
            Filter 'FMin p (x ': xs) = If (Cmp x p == 'LT) (x ': (Filter 'FMin p xs)) (Filter 'FMin p xs)
            Filter 'FMax p (x ': xs) = If (Cmp x p == 'GT || Cmp x p == 'EQ) (x ': (Filter 'FMax p xs)) (Filter 'FMax p xs)

type family Cmp (a :: k) (b :: k) :: Ordering
