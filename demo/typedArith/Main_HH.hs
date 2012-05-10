{-# LANGUAGE KindSignatures, TypeOperators, GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeSynonymInstances #-}

module Main where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V

import Control.Monad
import Data.Char

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

instance (VFunctor (y a) (y b) x, VFunctor a b y) => VFunctor a b (Data (x :> y :> Nil)) where
         vmap f (Data (S2 xs)) = Data (S2 (vmap (vmap f) xs))

instance (VFunctor (y (z a)) (y (z b)) x, VFunctor (z a) (z b) y, VFunctor a b z)
         => VFunctor a b (Data (x :> y :> z :> Nil)) where
         vmap f (Data (S3 xs)) = Data (S3 (vmap (vmap (vmap f)) xs))


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

func :: (Ord a, Num a, VFunctor a Bool f) => a -> f a -> f Bool
func x xs = vmap p xs
  where f p = (x < y) && (y < 2*x)

main :: IO ()
main = do
  print (func 4 v3)
  print (func 4 v4)
  print (func 4 v5)
  print (func 4 v6)
  print (func 4 v7)
  print (func 4 v8)
  print (func 4 v9)