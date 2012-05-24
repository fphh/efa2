{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances, KindSignatures, GeneralizedNewtypeDeriving, FlexibleContexts #-} 


module EFA2.Signal.Vector (module EFA2.Signal.Vector) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV

import Data.Monoid
import Control.Applicative
import EFA2.Utils.Utils
import qualified Data.List as L 

import EFA2.Signal.Base


---------------------------------------------------------
-- | 2. Data Structures with 
 
-- type Vec  = V.Vector -- deriving Show
-- type UVec = UV.Vector -- deriving Show
-- type List = [] -- deriving Show
-- newtype Value a = Value a -- deriving Show

data Vector v = Vector v | VZipErr v | VTransErr v

type VUVec a = Vector (UV.Vector a)

vunpack :: Vector v -> v
vunpack (Vector x) = x 

vunpack2 xs = vunpack $ vmap vunpack xs

  
--------------------------------------------------------------
-- Singleton Class

class VSingleton vec d where
      vmaximum :: Vector (vec d) -> d
      vminimum :: Vector (vec d) -> d
      vsingleton :: d -> Vector (vec d)
      vempty :: Vector (vec d)
      vappend :: Vector (vec d) -> Vector (vec d) -> Vector (vec d)
      vconcat :: [Vector (vec d)] -> Vector (vec d)
      vhead :: Vector (vec d) -> d
      vtail :: Vector (vec d) -> Vector (vec d)
      vlast :: Vector (vec d) -> d
      vinit :: Vector (vec d) -> Vector (vec d)

instance (Ord d) => VSingleton V.Vector d where 
         vmaximum (Vector x) =  V.maximum x
         vminimum (Vector x) =  V.minimum x
         vsingleton x = Vector $ V.singleton x
         vempty = Vector $ V.empty
         vappend (Vector x) (Vector y) = Vector $ x V.++ y
         vconcat xs = Vector $ V.concat xss
                      where xss = map vunpack xs
         vhead (Vector x) = V.head x
         vtail (Vector x) = Vector $ V.tail x
         vlast (Vector x) = V.last x
         vinit (Vector x) =  Vector $ V.init x

instance (Ord d, UV.Unbox d) => VSingleton UV.Vector d where 
         vmaximum (Vector x) =  UV.maximum x
         vminimum (Vector x) =  UV.minimum x
         vsingleton x = Vector $ UV.singleton x
         vempty = Vector $ UV.empty
         vappend (Vector x) (Vector y) = Vector $ x UV.++ y
         vconcat xs = Vector $ UV.concat xss
                      where xss = map vunpack xs
         vhead (Vector x) = UV.head x
         vtail (Vector x) = Vector $ UV.tail x
         vlast (Vector x) = UV.last x
         vinit (Vector x) =  Vector $ UV.init x

instance (Ord d) => VSingleton [] d where 
         vmaximum (Vector x) =  maximum x
         vminimum (Vector x) =  minimum x
         vsingleton x = Vector $ [x]
         vempty = Vector $ empty
         vappend (Vector x) (Vector y) = Vector $ x ++ y
         vconcat xs = Vector $ concat xss
                      where xss = map vunpack xs
         vhead (Vector x) = head x
         vtail (Vector x) = Vector $ tail x
         vlast (Vector x) = last x
         vinit (Vector x) =  Vector $ init x

------------------------------------------------------------
-- | Functor
class VWalker vec a b where
      vmap :: (a -> b) -> Vector (vec a) -> Vector (vec b)
      vfoldr :: (a -> b -> b) -> b -> Vector (vec a) -> b
      vfoldl :: (a -> b -> a) -> a -> Vector (vec b) -> a
--      vzip :: Vector (vec a) -> vec b -> vec (a, b)

instance VWalker [] a b where
         vmap f (Vector x) = Vector $ map f x
         vfoldr f a (Vector x) =  foldr f a x
         vfoldl f a (Vector x) =  L.foldl' f a x
--         vzip (Vector x) (Vector x) = Vector $ zip x

instance (UV.Unbox a, UV.Unbox b) => VWalker UV.Vector a b where
         vmap f (Vector x) = Vector $ UV.map f x
         vfoldr f a (Vector x) = UV.foldr f a x
         vfoldl f a (Vector x) = UV.foldl' f a x
--         vzip (Vector x) = Vector $ UV.zip x

instance VWalker V.Vector a b where
         vmap f (Vector x) = Vector $ V.map f x
         vfoldr f a (Vector x) = V.foldr f a x
         vfoldl f a (Vector x) = V.foldl' f a x
--         vzip (Vector x) = Vector $ V.zip x


------------------------------------------------------------
-- | Zipper

class VZipper vec a b c where
      vzipWith :: (a -> b -> c) -> Vector (vec a) -> Vector (vec b) -> Vector (vec c)
      
instance VZipper V.Vector a b c  where
         vzipWith f (Vector x) (Vector y) | vlenCheck x y = Vector $ V.zipWith f x y 
                                 | otherwise = VZipErr $ V.zipWith f x y


instance (UV.Unbox a, UV.Unbox b, UV.Unbox c) => VZipper UV.Vector a b c  where
         vzipWith f (Vector x) (Vector y) | vlenCheck x y = Vector $ UV.zipWith f x y 
                                 | otherwise = VZipErr $ UV.zipWith f x y

instance VZipper [] a b c  where
         vzipWith f (Vector x) (Vector y) | vlenCheck x y = Vector $ zipWith f x y 
                                 | otherwise = VZipErr $ zipWith f x y  

-- instance VZipper Value a b c  where
--         vzipWith f (Value x) (Value y) = Value (f x y)


vdeltaMap :: (VSingleton vec b, VZipper vec b b c) => (b -> b -> c) -> Vector (vec b) -> Vector (vec c)
vdeltaMap f l = vzipWith f l (vtail l)

vdeltaMapReverse :: (VSingleton vec b, VZipper vec b b c) => (b -> b -> c) -> Vector (vec b) -> Vector (vec c)
vdeltaMapReverse f l = vzipWith f (vtail l) l


--------------------------------------------------------------
-- Vector conversion
class VBox c1 c2 a where
      vbox :: Vector (c1 a) -> Vector (c2 a)
      vunbox :: Vector (c2 a) -> Vector (c1 a)

instance UV.Unbox a => VBox UV.Vector V.Vector a where
         vbox (Vector x) = Vector $ UV.convert x
         vunbox (Vector x) = Vector $ V.convert x

instance VBox [] [] a where
         vbox = id
         vunbox = id
  
--------------------------------------------------------------
-- Length & Length Check

class GetLength v where
  vlen :: v -> Int

instance GetLength  (V.Vector d) where 
  vlen x =  V.length x

instance UV.Unbox d => GetLength  (UV.Vector d) where 
  vlen x = UV.length x

instance GetLength  [d] where 
  vlen x = length x

vlenCheck :: (GetLength v1,GetLength v2) => v1 -> v2 -> Bool
vlenCheck x y = vlen x == vlen y 


--------------------------------------------------------------
-- Transpose Classe
class VTranspose v1 v2 d where
      vtranspose :: Vector (v2 ( Vector (v1 d))) -> Vector (v2 (Vector (v1 d)))


instance VTranspose V.Vector V.Vector d where
  vtranspose (Vector xs) = if V.all (== len) lens then Vector $ V.map Vector txs else VTransErr $ V.map Vector txs
    where fs = V.map (flip (V.!)) $ V.fromList [0..len-1]
          lens = V.map vlen xss  
          len = V.head lens  
          xss = V.map vunpack xs
          txs = V.map (flip V.map xss) fs
{-    
instance (UV.Unbox d) =>  VTranspose  UV.Vector V.Vector d where
  vtranspose (Vector xs) = if V.all (== len) lens then Vector $ V.map Vector txs  else VTransErr $ V.map Vector txs
    where fs = V.map (flip (UV.!)) $ V.fromList [0..len-1]
          lens = V.map vlen xss  
          len = V.head lens  
          xss = V.map vunpack xs
          txs = V.map (vunbox . (flip V.map) xss) fs

instance VTranspose [] [] d where
  vtranspose (Vector xs) = if all (== head lens) lens then Vector $ map Vector txs else VTransErr $ map Vector txs
                         where lens = map vlen xss
                               xss = V.map vunpack xs
                               txs =  L.transpose xss



-}