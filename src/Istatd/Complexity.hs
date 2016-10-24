{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Istatd.Complexity where


import            Data.Void                               (Void)

class Summable (fs :: [* -> *]) where
  data Summed fs :: * -> *

instance Summable '[] where
  data Summed '[] a = SummedNil Void
                    deriving Functor

instance Summable (f ': fs) where
  data Summed (f ': fs) a = Functor f => Here (f a)
                          | Elsewhere (Summed fs a)

deriving instance Functor (Summed fs) => Functor (Summed (f': fs))

class Injectable (f :: * -> *) (fs :: [* -> *]) where
  inj :: f a -> Summed fs a

instance Functor f => Injectable f (f ': fs) where
  inj = Here

instance {-# OVERLAPPABLE #-} Injectable f fs => Injectable f (g ': fs) where
  inj = Elsewhere . inj

class Outjectable (f :: * -> *) (fs :: [* -> *]) where
  outj :: Summed fs a -> Maybe (f a)

instance Outjectable f (f ': fs) where
  outj (Here fa)     = Just fa
  outj (Elsewhere _) = Nothing

instance {-# OVERLAPPABLE #-} Outjectable f fs => Outjectable f (g ': fs) where
  outj (Here _)         = Nothing
  outj (Elsewhere fa)   = outj fa

class ( Summable fs
      , Injectable f fs
      , Outjectable f fs
      , Functor (Summed fs)
      ) => (f :: * -> *) :<: (fs :: [* -> *])
instance ( Summable fs
         , Injectable f fs
         , Outjectable f fs
         , Functor (Summed fs)
         ) => (f :<: fs)
