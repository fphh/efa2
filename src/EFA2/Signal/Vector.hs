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
 
type Vec  = V.Vector -- deriving Show
type UVec = UV.Vector -- deriving Show
type List = [] -- deriving Show
newtype Value a = Value a -- deriving Show
  
------------------------------------------------------------
-- | Functor

class VFunctor a b f where
  vmap :: (a -> b) -> f a -> f b  
  
instance VFunctor a b V.Vector where
         vmap = V.map

instance (UV.Unbox a, UV.Unbox b) => VFunctor a b UV.Vector where
         vmap = UV.map

instance VFunctor a b [] where
         vmap = map

instance VFunctor a b Value where
         vmap f (Value x) = Value $ f x

------------------------------------------------------------
-- | Zipper

class VZipper a b c f where
      vzipWith :: (a -> b -> c) -> f a -> f b -> f c
      
instance VZipper a b c V.Vector where
         vzipWith = V.zipWith

instance (UV.Unbox a, UV.Unbox b, UV.Unbox c) => VZipper a b c UV.Vector where
         vzipWith = UV.zipWith

instance VZipper a b c [] where
         vzipWith = zipWith

instance VZipper a b c Value where
         vzipWith f (Value x) (Value y) = Value (f x y)

-------------------------------------------------------------
-- fold   
{-         
class VFold a c1 c2 where
      vfoldl :: (acc -> a -> acc) -> acc -> a -> acc 
      vfoldr :: (a -> acc -> acc) -> acc -> a -> acc

instance UV.Unbox a => VFold a (Data Nil) UV.Vector where
         vfoldl = UV.foldl'
         vfoldr = UV.foldr

instance VFold a (Data Nil) V.Vector where
         vfoldl = V.foldl'
         vfoldr = V.foldr

instance VFold a (Data Nil)  [] where
         vfoldl = L.foldl'
         vfoldr = foldr
-}
---------------------------------------------------------------
-- Monoid
class VMonoid c d where
 vempty :: (c d)
 (.++) :: (c d) -> (c d) -> (c d)
 
instance VMonoid List d where 
  vempty = [] 
  (.++) = (++)

instance VMonoid Vec d where 
  vempty = V.empty
  (.++) = (V.++)

instance UV.Unbox d => VMonoid UVec d where 
  vempty = UV.empty
  (.++) = (UV.++)

--------------------------------------------------------------
-- Vector conversion
class VConvert a c1 c2 where
      vbox :: c1 a -> c2 a
      vunbox :: c2 a -> c1 a

instance UV.Unbox a => VConvert a UV.Vector V.Vector where
         vbox x = UV.convert x
         vunbox x = V.convert x

instance VConvert a [] [] where
         vbox = id
         vunbox = id
  
--------------------------------------------------------------
-- Length & Length Check

class GetLength s where
  vlen :: (s) -> Int

instance GetLength  (Vec d) where 
  vlen x =  V.length x

instance UV.Unbox d => GetLength  (UVec d) where 
  vlen x = UV.length x

instance GetLength  (List d) where 
  vlen x = length x

vlenCheck x y = vlen x == vlen y






