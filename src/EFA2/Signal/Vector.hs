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
newtype Value a = Value a -- deriving Show
  
------------------------------------------------------------
-- | Functor
class VWalker vec a b where
      vmap :: (a -> b) -> vec a -> vec b
      vfoldr :: (a -> b -> b) -> b -> vec a -> b
      vfoldl :: (a -> b -> a) -> a -> vec b -> a
      vzip :: vec a -> vec b -> vec (a, b)
      vdeltaMap :: (a -> a -> b) -> vec a -> vec b
      vdeltaMap' :: (a -> a -> b) -> vec a -> vec b

instance VWalker [] a b where
         vmap = map
         vfoldr = foldr
         vfoldl = L.foldl'
         vzip = zip
         vdeltaMap f l = zipWith f (init l) (tail l)
         vdeltaMap' f l = zipWith f (tail l) (init l)

instance (UV.Unbox a, UV.Unbox b) => VWalker UV.Vector a b where
         vmap = UV.map
         vfoldr = UV.foldr
         vfoldl = UV.foldl'
         vzip = UV.zip
         vdeltaMap f l = UV.zipWith f (UV.init l) (UV.tail l)  
         vdeltaMap' f l = UV.zipWith f (UV.tail l) (UV.init l)

instance VWalker V.Vector a b where
         vmap = V.map
         vfoldr = V.foldr
         vfoldl = V.foldl'
         vzip = V.zip
         vdeltaMap f l = V.zipWith f (V.init l) (V.tail l)  
         vdeltaMap' f l = V.zipWith f (V.tail l) (V.init l)

-- instance VFunctor a b Value where
--          vmap f (Value x) = Value $ f x


------------------------------------------------------------
-- | Zipper

class VZipper vec a b c where
      vzipWith :: (a -> b -> c) -> vec a -> vec b -> vec c
      
instance VZipper V.Vector a b c  where
         vzipWith = V.zipWith

instance (UV.Unbox a, UV.Unbox b, UV.Unbox c) => VZipper UV.Vector a b c  where
         vzipWith = UV.zipWith

instance VZipper [] a b c  where
         vzipWith = zipWith

instance VZipper Value a b c  where
         vzipWith f (Value x) (Value y) = Value (f x y)

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
  vlen :: (s) -> Int

instance GetLength  (V.Vector d) where 
  vlen x =  V.length x

instance UV.Unbox d => GetLength  (UV.Vector d) where 
  vlen x = UV.length x

instance GetLength  [d] where 
  vlen x = length x

vlenCheck x y = vlen x == vlen y


--------------------------------------------------------------
-- TRanspose Classe
class VTRanspose v1 v2 d where
      vtranspose :: (v2 (v1 d)) -> (v2 (v1 d))


instance VTRanspose V.Vector V.Vector d where
  vtranspose xs = V.map (flip V.map xs) fs
            where fs = V.take min $ V.map (flip (V.!)) $ V.fromList [0..]
                  min = V.minimum $ V.map V.length xs

instance (UV.Unbox d) =>  VTRanspose  UV.Vector V.Vector d where
  vtranspose xs = V.map (vunbox . flip V.map xs) fs
            where fs = V.take min $ V.map (flip (UV.!)) $ V.fromList [0..]
                  min = V.minimum $ V.map UV.length xs
         -- vtranspose [] = []
         -- vtranspose xs = map (UV.fromList . flip map xs) fs
         --   where fs = take min $ map (flip (UV.!)) [0..]
         --         min = L.minimum $ map UV.length xs

instance VTRanspose [] [] d where
  vtranspose = L.transpose

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
         -- vtranspose [] = []
         -- vtranspose xs = V.map (xs V.!) (V.fromList [0..])
         --   where fs = take min $ map (flip (V.!)) [0..]
         --         min = L.minimum $ map V.length xs
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
         -- vtranspose [] = []
         -- vtranspose xs = map (UV.fromList . flip map xs) fs
         --   where fs = take min $ map (flip (UV.!)) [0..]
         --         min = L.minimum $ map UV.length xs
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
--         vtranspose = L.transpose
         vhead = head
         vtail = tail
         vlast = last
         vinit = init






