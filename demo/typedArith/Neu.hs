{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, FlexibleContexts, OverlappingInstances, UndecidableInstances, FlexibleContexts #-}


import qualified Data.Vector.Unboxed as UV

import Data.Monoid
--import Data.Vector
import Control.Applicative


-- newtype EVal   d = EVal d deriving (Show)
-- newtype EList   d = EList [d] deriving (Show) 
-- newtype EList2  d = EList2 [[d]]  deriving (Show)
-- newtype EVec    d = EVec (Vec d)  deriving (Show)
-- newtype EVec2   d = EVec2 (Vec (Vec d))  deriving (Show)
-- newtype EUVec d =  EUVec (UVec d) deriving (Show)



newtype C df val = C (df val) deriving (Show)


newtype Scalar a = Scalar a deriving (Show, Eq, Num)

-- v1 :: C Signal P [] Int
-- v1 = C [1..10]

-- v3 :: C Signal P UV.Vector Int
-- v3 = C (UV.fromList [1..10])

-- v4 :: C Signal P UV.Vector Bool
-- v4 = C (UV.fromList (True:(replicate 9 False)))

-- v2 :: C Value DT Scalar Int
-- v2 = C 7



instance Functor Scalar where
  fmap f (Scalar x) = Scalar (f x)
  
class DF (df1 :: * -> *) (df2 :: * -> *) (df3 :: * -> *) | df1 df2 -> df3 where
  
instance DF [] Scalar []



class G e1 d1 e2 d2 e3 d3 where
  g :: (d1 -> d2 -> d3) -> e1 d1 -> e2 d2 -> e3 d3
  
instance (UV.Unbox a1, UV.Unbox a2, UV.Unbox a3) => G UV.Vector a1 UV.Vector a2 UV.Vector a3 where
  g f as bs = UV.zipWith f as bs

instance G [] a1 [] a2 [] a3 where
  g f as bs = zipWith f as bs

  
class DMult a b c where
  (.*) :: a -> b -> c
  
instance DMult Int Int Int where
  (.*) = (*)


class TMult t1 t2 t3 where
  mult :: t1 -> t2 -> t3

instance (G u a v b w c, DMult a b c) => TMult (C u a) (C v b) (C w c) where
  mult (C vec) (C x) = C (g (.*) vec x)


data P
data DT
data E

class PMult a b c where
  (..*) ::  a -> b -> c

instance (TMult a b c) => PMult (TC P a) (TC DT b) (TC E c) where
  (TC x) ..* (TC y) = TC (mult x y)
  
newtype TC t a = TC a deriving (Show)

v1 :: TC P (C [] Int)
v1 = TC (C [1, 2, 3])

v2 :: TC DT (C [] Int)
v2 = TC (C [4, 5, 6])

v3 :: TC P (C UV.Vector Int)
v3 = TC (C (UV.fromList [1, 2, 3]))

v4 :: TC DT (C UV.Vector Int)
v4 = TC (C (UV.fromList [4, 5, 6]))



main :: IO ()
main = do
  print v3
  print v4
  print (v1 ..* v2 :: TC E (C [] Int))

{-
class TAdd t1 t2 t3 where
  aaa :: t1 -> t2 -> t3
                       
instance (H (u a) (v b) (w c)) => TAdd (C u a) (C v b) (C w c) where
  aaa (C vec) (C x) = C (h vec x)
                       
class MultAdd a b c where
  mul :: a -> b -> c
  add :: a -> b -> c

instance (TAdd a b c, TMult a b c) => MultAdd a b c where
  mul = mult
  add = aaa

-}
{-
instance (Num a, Phys x y z, UV.Unbox a) => TMult (C c1 x UV.Vector a) (C c2 y UV.Vector Bool) (C c3 z UV.Vector a) where
  mult (C vec) (C bvec) = C (g bvec vec)
    where f True y = y
          f False y = 0
          g a as = UV.zipWith f a as

instance (Num a, Phys x y z, DF df1 Scalar df1, Functor df1) => TMult (C c1 x df1 a) (C c2 y Scalar a) (C c3 z df1 a) where
  mult (C xs) (C (Scalar x)) = C (g x xs)
    where g a as = fmap (*a) as)


instance Functor (C Signal P []) where
  fmap f (C vec) = C (map f vec)
  
instance Applicative (C Signal P []) where
  pure x = C [x]
  (C f) <*> (C x) = C (zipWith ($) f x)
  
-}


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

