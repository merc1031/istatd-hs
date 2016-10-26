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
module Istatd.SimplicityTree where


--import            Data.Void                               (Void)

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat


infixr 6 :+:

data (f :+: g) = InL f
               | InR g

--class Subsume (f :: *) (fs :: [*]) where
--  injF :: f -> Summed fs
--  outjF :: Summed fs -> Maybe f

data Crumbs = H | L Crumbs | R Crumbs
data Res = Found Crumbs | NotFound | Ambiguous



type family Elem e fs :: Res where
  Elem e e           = 'Found 'H
  Elem e (f :+: g)   = Choose (Elem e f) (Elem e g)
  Elem e f           = 'NotFound

type family Choose e f :: Res where
  Choose ('Found p) ('Found p')     = 'Ambiguous
  Choose 'Ambiguous   f             = 'Ambiguous
  Choose e           'Ambiguous     = 'Ambiguous
  Choose ('Found p) f               = 'Found ('L p)
  Choose e ('Found p)               = 'Found ('R p)
  Choose e f                        = 'NotFound

class Subsume (res :: Res) f g where
  inj' :: f -> g
  prj' :: g -> Maybe f

instance Subsume ('Found 'H) f f where
  inj' = id
  prj' = Just

instance (Subsume ('Found p) f l) => Subsume ('Found ('L p)) f (l :+: r) where
  inj' = InL . inj' @('Found p)
  prj' (InL x) = prj' @('Found p) x
  prj' (InR _) = Nothing

instance (Subsume ('Found p) f r) => Subsume ('Found ('R p)) f (l :+: r) where
  inj' = InR . inj' @('Found p)
  prj' (InR x) = prj' @('Found p) x
  prj' (InL _) = Nothing


data Struc = Sum Struc Struc | Atom Res

type family GetStruc f g :: Struc where
  GetStruc (f1 :+: f2) g = 'Sum (GetStruc f1 g) (GetStruc f2 g)
  GetStruc f           g = 'Atom (Elem f g)


class Subsume' (s :: Struc) f g where
  inj'' :: f -> g
  prj'' :: g -> Maybe f

instance Subsume res f g => Subsume' ('Atom res) f g where
  inj'' x = inj' @res x
  prj'' x = prj' @res x

instance (Subsume' s1 f1 g, Subsume' s2 f2 g) => Subsume' ('Sum s1 s2) (f1 :+: f2) g where
  inj'' (InL x) = inj'' @s1 x
  inj'' (InR x) = inj'' @s2 x
  prj'' x = case prj'' @s1 x of
              Just y -> Just (InL y)
              Nothing -> case prj'' @s2 x of
                           Just y -> Just (InR y)
                           Nothing -> Nothing

type family Or l r :: Bool where
  Or 'False 'False = 'False
  Or _ _           = 'True

type family Dupl f l :: Bool where
  Dupl (f :+: g) l = Dupl f (g ': l)
  Dupl f         l = Or (Find f l) (Dupl' l)

type family Dupl' l :: Bool where
  Dupl' (f ': l) = Or (Dupl f l) (Dupl' l)
  Dupl' '[]      = 'False

type family Find f l :: Bool where
  Find f (g ': l)    = Or (Find' f g) (Find f l)
  Find f '[]         = 'False

type family Find' f g :: Bool where
  Find' f (g1 :+: g2) = Or (Find' f g1) (Find' f g2)
  Find' f f           = 'True
  Find' f g           = 'False

infixl 5 :<:

type f :<: g = ( Subsume' (GetStruc f g) f g
               , NoDupl f (Dupl f '[])
               , NoDupl g (Dupl g '[])
               )

class NoDupl f s
instance NoDupl f 'False

infixl 5 :~:

type f :~: g = (f :<: g, g :<: f)

split :: (f :~: f1 :+: f2) => (f1 -> b) -> (f2 -> b) -> f -> b
split f1 f2 x = case inj x of
                  InL y -> f1 y
                  InR y -> f2 y

inj :: forall f g. (f :<: g) => f -> g
inj = inj'' @(GetStruc f g)

prj :: forall f g. (f :<: g) => g -> Maybe f
prj = prj'' @(GetStruc f g)
--test :: forall (sub :: [*]) (sum :: [*]) e
--      . ( Show e
--        , SubsumedBy sub sum
--        , Subsume e sum
--        )
--     => Summed sub
--     -> IO ()
--test v = case ((inj' @(sub) @(sum) $ (v ))) of
--  Just summed -> case outj summed of
--    Just (a :: e) -> putStrLn $ show a
--    Nothing -> return ()
--  Nothing -> return ()

--testSomeSubsumed :: forall (choose :: [*]) (sum :: [*])
--                  . ( SomeSubsumed choose sum
--                    )
--                 => Summed sum
--                 -> IO ()
--testSomeSubsumed v = case soutj @choose @sum v of
--  Just _ -> putStrLn "found it"
--  Nothing -> return ()
--testSomeSubsumed1 :: forall (choose :: *) (sum :: [*])
--                  . ( SomeSubsumed1 choose sum
--                    )
--                 => Summed sum
--                 -> IO ()
--testSomeSubsumed1 v = case soutj1 @choose @sum v of
--  Just _ -> putStrLn "found it"
--  Nothing -> return ()
--
--testSomeSubsumed2 :: forall (choose :: (*,*)) (sum :: [*]) n
--                  . ( SomeSubsumed2 n choose sum
--                    )
--                 => Summed sum
--                 -> IO ()
--testSomeSubsumed2 v = case soutj2 @choose @sum v of
--  Just _ -> putStrLn "found it"
--  Nothing -> return ()
--
--testSomeSubsumed3 :: forall (choose :: (*,*,*)) (sum :: [*])
--                  . ( SomeSubsumed3 choose sum
--                    )
--                 => Summed sum
--                 -> IO ()
--testSomeSubsumed3 v = case soutj3 @choose @sum v of
--  Just _ -> putStrLn "found it"
--  Nothing -> return ()

--testCases :: IO ()
--testCases = do
--  putStrLn "Should print 1, int 2nd position to 1st"
--  test @'[String,Int] @'[Int,String] @Int (Elsewhere $ Here 1)
--  putStrLn "Should not print, String not int"
--  test @'[String,Int] @'[Int,String] @Int (Here "asd")
--  putStrLn "Should  print asd, String 1st position to 2nd"
--  test @'[String,Int] @'[Int,String] @String (Here "asd")
--  putStrLn "Should print 1 identity"
--  test @'[String,Int] @'[String,Int] @Int (Elsewhere $ Here 1)
--  putStrLn "should print 26, int is 2nd position in both lists"
--  test @'[String,Int] @'[String,Int,Float] @Int (Elsewhere $ Here 26)
--  putStrLn "Should print asd identity"
--  test @'[String] @'[String] @String (Here "asd")
--  --wont compile, left is not subsumed by right
--  --test @'[String,Int,Float] @'[String,Int] @Int (Elsewhere $ Here 26)
----  testSomeSubsumed1 @String @'[String] (Here "asd")
----  testSomeSubsumed1 @String @'[Int, String] (Elsewhere $ Here "asd")
----  testSomeSubsumed1 @String @'[String, Int] (Here "asd")
--  testSomeSubsumed2 @'(String, Int) @'[String] (Here "asd")
--  testSomeSubsumed2 @'(String, Int) @'[Int] (Here 1)
--
--  want to find a float or an int in the larger channel
--   of int string float
--   fuck.will find both .. should make a Index 1 and 3 Summed '[Int,Float]
--  testSomeSubsumed2 @'[Float, Int] @'[Int,String,Float] (Here 1)
--
--  want to find a string or an int in the larger channel
--   of int char float
--   should find Int at index 1  Summed '[Int]
--testSomeSubsumed @'[String, Int] @'[Int,Char,Float] (Here 1)
