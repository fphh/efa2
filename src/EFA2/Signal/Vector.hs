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


--------------------------------------------------------------
-- Singleton Class

class VSingleton vec d where
      vmaximum :: (vec d) -> d
      vminimum :: (vec d) -> d
      vsingleton :: d -> vec d
      vempty :: (vec d)
      vappend :: vec d -> vec d -> vec d
      vconcat :: [vec d] -> vec d
--      vtranspose :: (vec1 (vec2 d)) -> (vec1 (vec2 d))
      vhead :: vec d -> d
      vtail :: vec d -> vec d
      vlast :: vec d -> d
      vinit :: vec d -> vec d

instance (Ord d) => VSingleton V.Vector d where 
         vmaximum x =  V.maximum x
         vminimum x =  V.minimum x
         vsingleton x = V.singleton x
         vempty = V.empty
         vappend = (V.++)
         vconcat = V.concat
         vhead = V.head
         vtail = V.tail
         vlast = V.last
         vinit = V.init

instance (Ord d, UV.Unbox d) => VSingleton UV.Vector d where 
         vmaximum x = UV.maximum x
         vminimum x = UV.minimum x
         vsingleton x = UV.singleton x
         vempty = UV.empty
         vappend = (UV.++)
         vconcat = UV.concat
         vhead = UV.head
         vtail = UV.tail
         vlast = UV.last
         vinit = UV.init

instance (Ord d) => VSingleton [] d where 
         vmaximum x = maximum x
         vminimum x = minimum x
         vsingleton x = [x]
         vempty = []
         vappend = (++)
         vconcat = concat
         vhead = head
         vtail = tail
         vlast = last
         vinit = init


------------------------------------------------------------
-- | Functor
class VWalker vec a b where
      vmap :: (a -> b) -> vec a -> vec b
      vfoldr :: (a -> b -> b) -> b -> vec a -> b
      vfoldl :: (a -> b -> a) -> a -> vec b -> a
      vzip :: vec a -> vec b -> vec (a, b)

instance VWalker [] a b where
         vmap = map
         vfoldr = foldr
         vfoldl = L.foldl'
         vzip = zip

instance (UV.Unbox a, UV.Unbox b) => VWalker UV.Vector a b where
         vmap = UV.map
         vfoldr = UV.foldr
         vfoldl = UV.foldl'
         vzip = UV.zip

instance VWalker V.Vector a b where
         vmap = V.map
         vfoldr = V.foldr
         vfoldl = V.foldl'
         vzip = V.zip


------------------------------------------------------------
-- | Zipper

class VZipper vec a b c where
      vzipWith :: (a -> b -> c) -> vec a -> vec b -> vec c
      
instance VZipper V.Vector a b c  where
         vzipWith f x y = V.zipWith f x y -- if vlenCheck x y then V.zipWith f x y else error "Error in vlenCheck V -- unequal Length" 

instance (UV.Unbox a, UV.Unbox b, UV.Unbox c) => VZipper UV.Vector a b c  where
         vzipWith  f x y = UV.zipWith f x y --if vlenCheck x y then UV.zipWith f x y else error "Error in vlenCheck UV -- unequal Length"

instance VZipper [] a b c  where
         vzipWith  f x y = zipWith f x y -- if vlenCheck x y then zipWith f x y else error "Error in vlenCheck List -- unequal Length"


vdeltaMap :: (VSingleton vec b, VZipper vec b b c) => (b -> b -> c) -> vec b -> vec c
vdeltaMap f l = vzipWith f l (vtail l)

vdeltaMapReverse :: (VSingleton vec b, VZipper vec b b c) => (b -> b -> c) -> vec b -> vec c
vdeltaMapReverse f l = vzipWith f (vtail l) l

------------------------------------------------------------
-- | Zipper4

class VZipper4 vec a b c d e where
      vzipWith4 :: (a -> b -> c -> d -> e) -> vec a -> vec b -> vec c -> vec d -> vec e
      
instance VZipper4 V.Vector a b c d e where
         vzipWith4 f w x y z = V.zipWith4 f w x y z -- if vlenCheck x y then V.zipWith f x y else error "Error in vlenCheck V -- unequal Length" 

instance (UV.Unbox a, UV.Unbox b, UV.Unbox c, UV.Unbox e, UV.Unbox d) => VZipper4 UV.Vector a b c d e   where
         vzipWith4  f w x y z = UV.zipWith4 f w x y z --if vlenCheck x y then UV.zipWith f x y else error "Error in vlenCheck UV -- unequal Length"

instance VZipper4 [] a b c d e  where
         vzipWith4  f w x y z = L.zipWith4 f w x y z -- if vlenCheck x y then zipWith f x y else error "Error in vlenCheck List -- unequal Length"

{-
vdeltaMap2:: (a -> a -> b -> b -> c)  -> vec a -> vec a -> vec b  -> vec b -> vec c
vdeltaMap2 f xs ys = vzipWith4 f xs (vtail xs) ys (vtail ys)

vdeltaMapReverse2 :: (a -> a -> b -> b -> c) -> vec a -> vec a -> vec b  -> vec b -> vec c
vdeltaMapReverse2 f xs ys = vzipWith4 f (vtail xs) xs (vtail ys) ys
-}
--------------------------------------------------------------
-- Vector conversion
class VBox c1 c2 a where
      vbox :: c1 a -> c2 a
      vunbox :: c2 a -> c1 a

instance UV.Unbox a => VBox UV.Vector V.Vector a where
         vbox x = UV.convert x
         vunbox x = V.convert x

instance VBox [] [] a where
         vbox = id
         vunbox = id
  
--------------------------------------------------------------
-- Length & Length Check

class GetLength s where
  vlen :: s -> Int

instance GetLength  (V.Vector d) where 
  vlen x =  V.length x

instance UV.Unbox d => GetLength  (UV.Vector d) where 
  vlen x = UV.length x

instance GetLength  [d] where 
  vlen x = length x


vlenCheck x y = vlen x == vlen y 


--------------------------------------------------------------
-- Transpose Classe
class VTranspose v1 v2 d where
      vtranspose :: (v2 (v1 d)) -> (v2 (v1 d))


instance VTranspose V.Vector V.Vector d where
  vtranspose xs = if V.all (== len) lens then V.map (flip V.map xs) fs else error "Error in VTranspose -- unequal length"
    where fs = V.map (flip (V.!)) $ V.fromList [0..len-1]
          lens = V.map vlen xs  
          len = V.head lens  
    
instance (UV.Unbox d) =>  VTranspose  UV.Vector V.Vector d where
  vtranspose xs = if V.all (== len) lens then V.map (vunbox . flip V.map xs) fs else error "Error in VTranspose -- unequal length"
    where fs = V.map (flip (UV.!)) $ V.fromList [0..len-1]
          lens = V.map vlen xs  
          len = V.head lens  

instance VTranspose [] [] d where
  vtranspose x = if all (== head lens) lens then  L.transpose x else error "Error in VTranspose -- unequal length"
                         where lens = map vlen x



