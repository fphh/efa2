{-# LANGUAGE KindSignatures, TypeOperators, GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeSynonymInstances #-}

module Main where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V

import Control.Monad
import Data.Char

import qualified Data.List as L 

-- | Newtype Wrapper data

newtype Data (a :: * -> *) b = Data (a b) deriving (Show)

data ((a :: * -> *) :> (b :: * -> *)) :: * -> * where
     S0 :: c -> Nil c 
     S1 :: b c -> (b :> Nil) c
     S2 :: a (b c) -> (a :> b :> Nil) c
     S3 :: a (b (c d)) -> (a :> b :> c :> Nil) d

instance (Show a) => Show (Nil a) where
         show (S0 x) = show x

instance (Show (a c)) => Show ((a :> Nil) c) where
         show (S1 x) = show x

instance (Show (a (b c))) => Show ((a :> b :> Nil) c) where
         show (S2 x) = show x

instance (Show (a (b (c d)))) => Show ((a :> b :> c :> Nil) d) where
         show (S3 x) = show x

infixr 9 :>

data Nil' c = Nil' deriving (Show)

type Nil = Nil' :> Nil'


-- | Functor

class VFunctor a b f where
  vmap :: (a -> b) -> f a -> f b  
  
instance VFunctor a b (Data Nil) where
         vmap f (Data (S0 x)) = Data (S0 (f x))
  
instance VFunctor a b V.Vector where
         vmap f vec = V.map f vec

instance (UV.Unbox a, UV.Unbox b) => VFunctor a b UV.Vector where
         vmap f vec = UV.map f vec

instance VFunctor a b [] where
         vmap f xs = map f xs

instance (VFunctor a b y) => VFunctor a b (Data (y :> Nil)) where
         vmap f (Data (S1 xs)) = Data (S1 (vmap f xs))

instance (VFunctor a b x) =>  VFunctor a b (Data (x :> y :> Nil)) where
         vmap f (Data (S2 xs)) = Data (S2 (vmap f xs))

{-
instance (VFunctor a b y) =>  VFunctor a b (Data (x :> y :> Nil)) where
         vmap f (Data (S2 xs)) = Data (S2 (vmap f xs))

instance (VFunctor (y (z a)) (y (z b)) x, VFunctor (z a) (z b) y, VFunctor a b z)
         => VFunctor a b (Data (x :> y :> z :> Nil)) where
         vmap f (Data (S3 xs)) = Data (S3 (vmap f xs))
-}

-- | Deep Functor

class VDeepFunctor a b f where
      vdeepmap :: (a -> b) -> f a -> f b

instance  (VFunctor a b (Data Nil)) => VDeepFunctor a b (Data Nil) where
         vdeepmap = vmap

instance (VFunctor a b y) => VDeepFunctor a b (Data (y :> Nil)) where
         vdeepmap = vmap

instance (VDeepFunctor (y a) (y b) x, VDeepFunctor a b y) => VDeepFunctor a b (Data (x :> y :> Nil)) where
         vdeepmap f (Data (S2 xs)) = Data (S2 (vdeepmap (vdeepmap f) xs))

instance (VDeepFunctor (y (z a)) (y (z b)) x, VDeepFunctor (z a) (z b) y, VDeepFunctor a b z)
         => VDeepFunctor a b (Data (x :> y :> z :> Nil)) where
         vdeepmap f (Data (S3 xs)) = Data (S3 (vdeepmap (vdeepmap (vdeepmap f)) xs))

-- | Deep Zipper

class VDeepZipper a b c f where
      vdeepZipWith :: (a -> b -> c) -> f a -> f b -> f c
      
instance VDeepZipper a b c (Data Nil) where
         vdeepZipWith f (Data (S0 x)) (Data (S0 y)) = Data (S0 (f x y))
         
instance VDeepZipper a b c V.Vector where
         vdeepZipWith f x y = V.zipWith f x y

instance (UV.Unbox a, UV.Unbox b, UV.Unbox c) => VDeepZipper a b c UV.Vector where
         vdeepZipWith f x y = UV.zipWith f x y

instance VDeepZipper a b c [] where
         vdeepZipWith f x y = zipWith f x y
     
instance (VDeepZipper a b c y) => VDeepZipper a b c (Data (y :> Nil)) where
         vdeepZipWith f (Data (S1 xs))  (Data (S1 ys)) = Data (S1 (vdeepZipWith f xs ys))

instance (VDeepZipper (y a) (y b) (y c) x, VDeepZipper a b c y) => VDeepZipper a b c (Data (x:> y :> Nil)) where
         vdeepZipWith f (Data (S2 xs))  (Data (S2 ys)) = Data (S2 (vdeepZipWith (vdeepZipWith f) xs ys))

instance (VDeepZipper (y (z a)) (y (z b)) (y (z c)) x,VDeepZipper (z a) (z b) (z c) y, VDeepZipper a b c z) => VDeepZipper a b c (Data (x:> y :> z :> Nil)) where
         vdeepZipWith f (Data (S3 xs))  (Data (S3 ys)) = Data (S3 (vdeepZipWith (vdeepZipWith (vdeepZipWith f)) xs ys))



-- Convert

class VConvert a c1 c2 where
      box :: c1 a -> c2 a
      unbox :: c2 a -> c1 a

instance VConvert a (Data Nil)  (Data Nil) where
         box = id
         unbox = id

instance UV.Unbox a => VConvert a UV.Vector V.Vector where
         box x = UV.convert x
         unbox x = V.convert x

instance VConvert a [] [] where
         box = id
         unbox = id

instance (VConvert a y1 y2) => VConvert a (Data (y1 :> Nil)) (Data (y2 :> Nil)) where
         box (Data (S1 xs)) = Data (S1 (box xs))
         unbox (Data (S1 xs)) = Data (S1 (unbox xs))

{-
instance (VConvert (y a) x1 x2,VConvert a y y) => VConvert a (Data (x1:> y1 :> Nil)) (Data (x2:> y2 :> Nil)) where
         box (Data (S2 xs)) = Data (S2 (box $ box xs))
         unbox (Data (S2 xs)) = Data (S2 (unbox $ unbox xs))
-}


class VFold a c1 c2 where
      vfoldl :: (acc -> a -> acc) -> c1 acc -> c2 a -> c1 acc 
      vfoldr :: (a -> acc -> acc) -> c1 acc -> c2 a -> c1 acc

instance UV.Unbox a => VFold a (Data Nil) UV.Vector where
         vfoldl f (Data (S0 accum)) x = Data (S0 (UV.foldl' f accum x))
         vfoldr f (Data (S0 accum)) x = Data (S0 (UV.foldr f accum x))

instance VFold a (Data Nil) V.Vector where
         vfoldl f (Data (S0 accum)) x = Data (S0 (V.foldl' f accum x))
         vfoldr f (Data (S0 accum)) x = Data (S0 (V.foldr f accum x))

instance VFold a (Data Nil)  [] where
         vfoldl f (Data (S0 accum)) x = Data (S0 (L.foldl' f accum x))
         vfoldr f (Data (S0 accum)) x = Data (S0 (foldr f accum x))
     
    


v3 :: Data Nil Integer
v3 = Data (S0 1)

v4 :: Data ([] :> Nil) Integer
v4 = Data (S1 [1..10])

v4' :: Data (UV.Vector :> Nil) Int
v4' = Data (S1 (UV.fromList [1..10]))


v5 :: Data ([] :> [] :> Nil) Integer
v5 = Data (S2 [[1..10]])

v6 :: Data ([] :> UV.Vector :> Nil) Int
v6 = Data (S2 [UV.fromList [1..10]])

v7 :: Data (V.Vector :> UV.Vector :> Nil) Int
v7 = Data (S2 (V.fromList [UV.fromList [1..10]]))

v8 :: Data ([] :> V.Vector :> UV.Vector :> Nil) Int
v8 = Data (S3 [V.fromList [UV.fromList [0..10], UV.fromList [7..19]] , V.fromList [UV.fromList [90..100]]])

v9 :: Data ([] :> [] :> [] :> Nil) Int
v9 = Data (S3 [[[1, 2], [3, 4]], [[7..10], [90..100]]]) 

{- ??? strange but correct syntax!
f :: Ord a => Num a => a -> Bool
f x = x < 9
-}

func :: (Ord a, Num a, VDeepFunctor a Bool f) => a -> f a -> f Bool
func x xs = vdeepmap f xs
  where f y = (x < y) && (y < 2*x)

main :: IO ()
main = do
  print (func 4 v3)
  print (func 4 v4)
  print (func 4 v5)
  print (func 4 v6)
  print (func 4 v7)
  print (func 4 v8)
  print (func 4 v9)