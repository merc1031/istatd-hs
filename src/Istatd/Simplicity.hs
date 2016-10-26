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
  inj                 = Here

  outj (Here fa)      = Just fa
  outj (Elsewhere _)  = Nothing

instance {-# OVERLAPPABLE #-} Subsume f fs => Subsume f (g ': fs) where
  inj                   = Elsewhere . inj

  outj (Here _)         = Nothing
  outj (Elsewhere fa)   = outj fa



-- This is another trick for bringing all constraints into scope
-- by duplicating them in class and single instance. from ekmett
class ( Summable fs
      , Subsume f fs
      ) => (f :: *) :<: (fs :: [*])
instance ( Summable fs
         , Subsume f fs
         ) => (f :<: fs)



class SubsumedBy (fs :: [*]) (ss :: [*]) where
  inj' :: Summed fs -> Maybe (Summed ss)
  outj' :: Summed ss -> Maybe (Summed fs)

--instance {-# OVERLAPS #-} SubsumedBy fs fs where
--        instance [overlap ok] forall (fs :: [*]). SubsumedBy fs fs
--        ...plus one instance involving out-of-scope types
--        (use -fprint-potential-instances to see them all)
--
-- Trick for overlappppp
instance {-# OVERLAPS #-} (fs ~ gs) => SubsumedBy fs gs where
  inj'    = Just . id
  outj'   = Just

instance {-# OVERLAPS #-} SubsumedBy fs os => SubsumedBy (f ': fs) (f ': os) where
  inj' (Here a)       = Just $ Here a
  inj' (Elsewhere a)  = Elsewhere <$> inj' a

  outj' (Here a)        = Just $ Here a
  outj' (Elsewhere _)   = Nothing

instance {-# OVERLAPPABLE #-} SubsumedBy '[] (g ': os) where
  inj' _    = Nothing

  outj' _   = Nothing

instance {-# OVERLAPPABLE #-} ( Subsume f os
                              , SubsumedBy fs (g ': os)
                              ) => SubsumedBy (f ': fs) (g ': os) where
  inj' (Here a)       = Just $ Elsewhere $ inj @f @os a
  inj' (Elsewhere a)  = inj' @fs @(g ': os) a

  outj' (Here _)        = Nothing
  outj' (Elsewhere a)   =
    case outj @f @os a of
      Just a' -> Just $ inj @f @(f ': fs) a'
      Nothing -> Nothing


-- This is another trick for bringing all constraints into scope
-- by duplicating them in class and single instance. from ekmett
class ( Summable sub
      , Summable sum
      , SubsumedBy sub sum
      ) => (sub :: [*]) :<<: (sum :: [*])
instance ( Summable sub
         , Summable sum
         , SubsumedBy sub sum
         ) => (sub :<<: sum)


class SomeSubsumed1 (choose :: *) (all :: [*]) where
  soutj1 :: Summed all -> Maybe (Summed '[choose])

instance SomeSubsumed1 c '[] where
  soutj1 _ = Nothing

instance SomeSubsumed1 c (c ': '[]) where
  soutj1 = Just . id

instance {-# OVERLAPPABLE #-} SomeSubsumed1 c (c ': cs) where
  soutj1 (Here a) = Just . inj $ a
  soutj1 (Elsewhere _) = Nothing

instance {-# OVERLAPPABLE #-} SomeSubsumed1 c (b ': '[]) where
  soutj1 _ = Nothing

instance {-# OVERLAPPABLE #-} (SomeSubsumed1 c cs) => SomeSubsumed1 c (b ': cs) where
  soutj1 (Here _) = Nothing
  soutj1 (Elsewhere a) = soutj1 a


class SomeSubsumed2 (choose :: (*,*)) (all :: [*]) where
  soutj2 :: Summed all -> Maybe (Summed (Uncurry choose))

instance SomeSubsumed2 '(c,c1) '[] where
  soutj2 _ = Nothing

instance SomeSubsumed2 '(c,c1) (c ': '[]) where
  soutj2 = Just . id

instance SomeSubsumed2 '(c1,c) (c ': '[]) where
  soutj2 = Just . id

instance {-# OVERLAPPABLE #-} SomeSubsumed2 '(c,c1) (c ': cs) where
  soutj2 (Here a) = Just . inj $ a
  soutj2 (Elsewhere _) = Nothing

instance {-# OVERLAPPABLE #-} SomeSubsumed2 '(c1,c) (c ': cs) where
  soutj2 (Here a) = Just . inj $ a
  soutj2 (Elsewhere _) = Nothing

instance {-# OVERLAPPABLE #-} SomeSubsumed2 '(c1,c) (b ': '[]) where
  soutj2 _ = Nothing

instance {-# OVERLAPPABLE #-} (SomeSubsumed2 '(c,c1) cs) => SomeSubsumed2 '(c,c1) (b ': cs) where
  soutj2 (Here _) = Nothing
  soutj2 (Elsewhere a) = soutj2 a

class SomeSubsumed3 (choose :: (*,*,*)) (all :: [*]) where
  soutj3 :: Summed all -> Maybe (Summed (Uncurry choose))

type family Uncurry (t :: k) :: [*] where
  Uncurry '(a,b) = '[a,b]
  Uncurry '(a,b,c) = '[a,b,c]
  Uncurry '(a,b,c,d) = '[a,b,c,d]

--class SomeSubsumed (choose :: [*]) (all :: [*]) where
--  soutj :: Summed all -> Maybe (Summed (choose))
--
--instance {-# OVERLAPS #-} (choose ~ all) => SomeSubsumed choose all where
--  soutj = Just . id
--
----instance {-# OVERLAPPABLE#-} ( Subsume c choose
----                             , Subsume c all
----                             , (c ': choose') ~ choose
----                             ) => SomeSubsumed (c ': choose') all where
----  soutj v = (inj @c @choose) <$> (outj @c @all v)
--instance {-# OVERLAPPABLE#-} ( (c ': choose') ~ choose
--                             , (c ': all') ~ all
--                             ) => SomeSubsumed (c ': choose') (c ': all') where
--
--  soutj v@(Here _) = (inj @c @choose) <$> (outj @c @all v)
--  soutj (Elsewhere a) = Nothing
--
--instance {-# OVERLAPPABLE#-} ( (c ': choose') ~ choose
--                             , (a ': all') ~ all
----                             , 'True ~ (InSS choose all)
--                             ) => SomeSubsumed (c ': choose') (a ': all') where
--
--  soutj v@(Here _) = (inj @c @choose) <$> (outj @c @all v)
--  soutj (Elsewhere a) = Nothing
--
--type family (||) (bl :: Bool) (br :: Bool) :: Bool where
--  'True || _     = 'True
--  _     || 'True = 'True
--  _     || _     = 'False
--
--type family InSS (smaller :: [*]) (larger :: [*]) :: Bool where
--  InSS (s ': smaller) larger = (InSS' s larger) || (InSS smaller larger)
--
--type family InSS' (s :: *) (larger :: [*]) :: Bool where
--  InSS' s (s ': larger') = 'True
--  InSS' s (g ': larger') = InSS' s larger'

test :: forall (sub :: [*]) (sum :: [*]) e
      . ( Show e
        , SubsumedBy sub sum
        , Subsume e sum
        )
     => Summed sub
     -> IO ()
test v = case ((inj' @(sub) @(sum) $ (v ))) of
  Just summed -> case outj summed of
    Just (a :: e) -> putStrLn $ show a
    Nothing -> return ()
  Nothing -> return ()

testSomeSubsumed1 :: forall (choose :: *) (sum :: [*])
                  . ( SomeSubsumed1 choose sum
                    )
                 => Summed sum
                 -> IO ()
testSomeSubsumed1 v = case soutj1 @choose @sum v of
  Just _ -> putStrLn "found it"
  Nothing -> return ()

testSomeSubsumed2 :: forall (choose :: (*,*)) (sum :: [*])
                  . ( SomeSubsumed2 choose sum
                    )
                 => Summed sum
                 -> IO ()
testSomeSubsumed2 v = case soutj2 @choose @sum v of
  Just _ -> putStrLn "found it"
  Nothing -> return ()

testSomeSubsumed3 :: forall (choose :: (*,*,*)) (sum :: [*])
                  . ( SomeSubsumed3 choose sum
                    )
                 => Summed sum
                 -> IO ()
testSomeSubsumed3 v = case soutj3 @choose @sum v of
  Just _ -> putStrLn "found it"
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
  testSomeSubsumed1 @String @'[String] (Here "asd")
  testSomeSubsumed1 @String @'[Int, String] (Elsewhere $ Here "asd")
  testSomeSubsumed1 @String @'[String, Int] (Here "asd")
  testSomeSubsumed2 @'(String, Int) @'[String] (Here "asd")
  testSomeSubsumed2 @'(String, Int) @'[Int] (Here 1)
