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

class Subsume (f :: *) (fs :: [*]) where
  inj :: f -> Summed fs
  outj :: Summed fs -> Maybe f

instance Subsume f (f ': fs) where
  inj = Here

  outj (Here fa)     = Just fa
  outj (Elsewhere _) = Nothing

instance {-# OVERLAPPABLE #-} Subsume f fs => Subsume f (g ': fs) where
  inj = Elsewhere . inj

  outj (Here _)         = Nothing
  outj (Elsewhere fa)   = outj fa

class ( Summable fs
      , Subsume f fs
      ) => (f :: *) :<: (fs :: [*])
instance ( Summable fs
         , Subsume f fs
         ) => (f :<: fs)

class Subsume' (fs :: [*]) (ss :: [*]) where
  inj' :: Summed fs -> Maybe (Summed ss)
  outj' :: Summed ss -> Maybe (Summed fs)

-- Trick for overlappppp
instance {-# OVERLAPS #-} (fs ~ gs) => Subsume' fs gs where
  inj' = Just . id
  outj' = Just

instance {-# OVERLAPPABLE #-} Subsume' fs os => Subsume' (f ': fs) (f ': os) where
  inj' (Here a) = Just $ Here a
  inj' (Elsewhere a) = Elsewhere <$> inj' a

  outj' (Here a) = Just $ Here a
  outj' (Elsewhere _) = Nothing

instance {-# OVERLAPPABLE #-} Subsume' '[] (g ': os) where
  inj' _ = Nothing

  outj' _ = Nothing

instance {-# OVERLAPPABLE #-} (Subsume f os, Subsume' fs (g ': os)) => Subsume' (f ': fs) (g ': os) where
  inj' (Here a) = Just $ Elsewhere $ inj @f @os a
  inj' (Elsewhere a) = inj' @fs @(g ': os) a
--    case outj a of
--                      Just a' -> Just $ inj a'
--                      Nothing -> Nothing
--  inj' (Elsewhere a) = Elsewhere <$> inj' a

  outj' (Here _) = Nothing
--  outj' (Here a) = case outj @g @os a of
--                     Just a' -> Just $ inj @g @fs a'
--                     Nothing -> Nothing
  outj' (Elsewhere a) = case outj @f @os a of
                          Just a' -> Just $ inj @f @(f ': fs) a'
                          Nothing -> Nothing

class ( Summable sub
      , Summable sum
      , Subsume' sub sum
      ) => (sub :: [*]) :<<: (sum :: [*])
instance ( Summable sub
         , Summable sum
         , Subsume' sub sum
         ) => (sub :<<: sum)
--class Outjectable' (fs :: [*]) (os :: [*]) where
--  outj' :: Summed fs -> Maybe (Summed os)
--
--instance Outjectable' (f ': fs) (f ': os) where
--  outj' (Here fa) = Just (inj fa)
--  outj' (Elsewhere _) = Nothing
--
--instance {-# OVERLAPPABLE #-} (f :<: os, fs :<<: os, Outjectable' fs os) => Outjectable' (f ': fs) (g ': os) where
--  outj' (Here fa) = Just $ inj fa
--  outj' (Elsewhere fa) = case outj' fa of
--                           Just a -> Just $ inj a
--                           Nothing -> Nothing


--class ( Summable fs
--      , Summable os
--      ) => (fs :: [*]) :<<: (os :: [*])
--instance ( 
--          Summable os
--         ) => (f ': fs) :<<: (os)

--class ForS (fs :: [*]) where
--  forS :: Monad m => fs -> (f -> m ()) -> m ()
--
--instance ForS (f ': fs) where
--  forS
--
test :: forall (sub :: [*]) (sum :: [*]) e
      . ( Show e
        , Subsume' sub sum
        , Subsume e sum
        )
     => Summed sub
     -> IO ()
test v = case ((inj' @(sub) @(sum)$ (v ))) of
    Just summed -> case outj summed of
        Just (a :: e) -> putStrLn $ show a
        Nothing -> return ()
    Nothing -> return ()


testCases :: IO ()
testCases = do
  putStrLn "Should print 1, int 2nd position to 1st"
  test @'[String,Int] @'[Int,String] @Int (Elsewhere $ Here 1)
  putStrLn "Should not print, String not int"
  test @'[String,Int] @'[Int,String] @Int (Here "asd")
  putStrLn "Should  print asd, String 1st position to 2nd"
  test @'[String,Int] @'[Int,String] @String (Here "asd")
  putStrLn "Should print 1 identity"
  test @'[String,Int] @'[String,Int] @Int (Elsewhere $ Here 1)
  putStrLn "should print 26, int is 2nd position in both lists"
  test @'[String,Int] @'[String,Int,Float] @Int (Elsewhere $ Here 26)
  putStrLn "Should print asd identity"
  test @'[String] @'[String] @String (Here "asd")
  --wont compile, left is not subsumed by right
  --test @'[String,Int,Float] @'[String,Int] @Int (Elsewhere $ Here 26)
