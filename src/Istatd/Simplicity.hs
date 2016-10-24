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
module Istatd.Simplicity where


import            Data.Void                               (Void)

class Summable (fs :: [*]) where
  data Summed fs :: *

instance Summable '[] where
  data Summed '[] = SummedNil Void

instance Summable (f ': fs) where
  data Summed (f ': fs) = Here f
                        | Elsewhere (Summed fs)

class Injectable (f :: *) (fs :: [*]) where
  inj :: f -> Summed fs

instance Injectable f (f ': fs) where
  inj = Here

instance {-# OVERLAPPABLE #-} Injectable f fs => Injectable f (g ': fs) where
  inj = Elsewhere . inj

class Outjectable (f :: *) (fs :: [*]) where
  outj :: Summed fs -> Maybe f

instance Outjectable f (f ': fs) where
  outj (Here fa)     = Just fa
  outj (Elsewhere _) = Nothing

instance {-# OVERLAPPABLE #-} Outjectable f fs => Outjectable f (g ': fs) where
  outj (Here _)         = Nothing
  outj (Elsewhere fa)   = outj fa

class ( Summable fs
      , Injectable f fs
      , Outjectable f fs
      ) => (f :: *) :<: (fs :: [*])
instance ( Summable fs
         , Injectable f fs
         , Outjectable f fs
         ) => (f :<: fs)
