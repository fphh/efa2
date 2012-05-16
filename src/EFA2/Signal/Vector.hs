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

-- -------------------------------------------------------------
-- -- fold   

-- class VFold v a where
--       vfoldl :: (acc -> a -> acc) -> acc -> v a -> acc 
--       vfoldr :: (a -> acc -> acc) -> acc -> v a -> acc

-- instance UV.Unbox a => VFold UV.Vector a where
--          vfoldl = UV.foldl'
--          vfoldr = UV.foldr

-- instance VFold V.Vector a where
--          vfoldl = V.foldl'
--          vfoldr = V.foldr

-- instance VFold [] a where
--          vfoldl = L.foldl'
--          vfoldr = foldr

-- ---------------------------------------------------------------
-- -- Monoid
-- class VMonoid c d where
--  vempty :: (c d)
--  (.++) :: (c d) -> (c d) -> (c d)
 
-- instance VMonoid V.Vector d where 
--   vempty = V.empty
--   (.++) = (V.++)

-- instance UV.Unbox d => VMonoid UV.Vector d where 
--   vempty = UV.empty
--   (.++) = (UV.++)

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

instance GetLength  (V.Vector d) where 
  vlen x =  V.length x

instance UV.Unbox d => GetLength  (UV.Vector d) where 
  vlen x = UV.length x

instance GetLength  [d] where 
  vlen x = length x

vlenCheck x y = vlen x == vlen y


--------------------------------------------------------------
-- Max & Min

class VSingleton vec d where
      vmaximum :: (vec d) -> d
      vminimum :: (vec d) -> d
      vsingleton :: d -> vec d
      vempty :: (vec d)
      vappend :: vec d -> vec d -> vec d
      vconcat :: [vec d] -> vec d
      vtranspose :: [vec d] -> [vec d]
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
         vtranspose [] = []
         vtranspose xs = map (V.fromList . flip map xs) fs
           where fs = take min $ map (flip (V.!)) [0..]
                 min = L.minimum $ map V.length xs
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
         vtranspose [] = []
         vtranspose xs = map (UV.fromList . flip map xs) fs
           where fs = take min $ map (flip (UV.!)) [0..]
                 min = L.minimum $ map UV.length xs
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
         vtranspose = L.transpose
         vhead = head
         vtail = tail
         vlast = last
         vinit = init






