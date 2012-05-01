{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, FlexibleContexts, OverlappingInstances, UndecidableInstances, FlexibleContexts #-}


import qualified Data.Vector.Unboxed as UV

import Data.Monoid
--import Data.Vector
import Control.Applicative




newtype C cont phys df val = C (df val) deriving (Show)

--type Sig = Signal (UV.Vector Val)

type Sig cont phys = C cont phys UV.Vector Int

newtype Scalar a = Scalar a deriving (Show, Eq, Num)

data Signal
data Value

data Zero
data Succ a

class Add a b ab | a b -> ab
instance Add Zero a a
instance Add a b ab => Add (Succ a) b (Succ ab)

data P
data DT
data E

data P' a

v1 :: C Signal P [] Int
v1 = C [1..10]

v3 :: C Signal P UV.Vector Int
v3 = C (UV.fromList [1..10])

v4 :: C Signal P UV.Vector Bool
v4 = C (UV.fromList (True:(replicate 9 False)))

v2 :: C Value DT Scalar Int
v2 = C 7



instance Functor Scalar where
  fmap f (Scalar x) = Scalar (f x)
  
class Container c1 c2 c3 | c1 c2 -> c3

class Phys x y z | x y -> z, x z -> y, y z -> x
instance Phys P DT E
instance (Phys a b c) => Phys b a c

instance (Add a b ab) => Phys (P' a) (P' b) (P' ab)


class DF (df1 :: * -> *) (df2 :: * -> *) (df3 :: * -> *) | df1 df2 -> df3 where
  
instance DF [] Scalar []


class TMult t1 t2 t3 where
  mult :: t1 -> t2 -> t3
  
type One = Succ Zero
p1 :: C Signal (P' One) [] Int
p1 = C [1, 2, 3]

p2 :: C Value (P' One) Scalar Int
p2 = C 8

instance (Num x, Add a b ab) => TMult (C c1 (P' a) [] x) (C c2 (P' b) Scalar x) (C c3 (P' ab) [] x) where
  mult (C xs) (C (Scalar x)) = C (fmap (*x) xs)
  

instance (Num a, Phys x y z, UV.Unbox a) => TMult (C c1 x UV.Vector a) (C c2 y Scalar a) (C c3 z UV.Vector a) where
  mult (C vec) (C (Scalar x)) = C (UV.map (*x) vec)

instance (Num a, Phys x y z, UV.Unbox a) => TMult (C c1 x UV.Vector a) (C c2 y UV.Vector Bool) (C c3 z UV.Vector a) where
  mult (C vec) (C bvec) = C (UV.zipWith f bvec vec)
    where f True y = y
          f False y = 0

instance (Num a, Phys x y z, DF df1 Scalar df1, Functor df1) => TMult (C c1 x df1 a) (C c2 y Scalar a) (C c3 z df1 a) where
  mult (C xs) (C (Scalar x)) = C (fmap (*x) xs)
  


instance Functor (C Signal P []) where
  fmap f (C vec) = C (map f vec)
  
instance Applicative (C Signal P []) where
  pure x = C [x]
  (C f) <*> (C x) = C (zipWith ($) f x)
  






{-
instance Functor UV.Vector where
  fmap f vec = UV.map f vec
-}

{-
newtype P a b = P b deriving (Show)

data Signal
data Value

instance Functor (P a) where
  fmap f (P x) = P (f x)

p1 :: P Signal (UV.Vector Int)
p1 = P (UV.fromList [4])

p2 :: P Value (UV.Vector Int)
p2 = P (UV.fromList [5])

class TSum t1 t2 t3 | t1 t2 -> t3 where
  func :: (Monoid m) => P t1 m -> P t2 m -> P t3 m

instance TSum Signal Value Signal where
  func (P x) (P y) = P (mappend x y)

instance TSum Value Signal Signal where
  func (P x) (P y) = P (mconcat [x, y, x, y])
-}


{-
instance Functor Signal where
  fmap f (Signal x) = Signal (f x)

newtype Value b = Value b deriving (Show)

instance Functor Value where
  fmap f (Value x) = Value (f x)

newtype P a = P a deriving (Show)

instance Functor P where
  fmap f (P x) = P (f x)

newtype E a = E a deriving (Show)

v :: P (Value (Vector Int))
v = P (Value (fromList [1, 2, 3]))


func :: (Functor f, Functor g) => (a -> b) -> P (g (f a)) -> P (g (f b))
func f x = fmap (fmap (fmap f)) x
-}