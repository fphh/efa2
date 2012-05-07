{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, GeneralizedNewtypeDeriving,FlexibleContexts,OverlappingInstances #-} 

module EFA2.Signal.Vector2 (module EFA2.Signal.Vector2) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV

import Data.Monoid
import Control.Applicative
import EFA2.Utils.Utils
import qualified Data.List as L 

import EFA2.Signal.Base


-- Phantom Type Flag to mark functions
data Unboxed
data Boxed


-- | Calculation classes for basic Datatypes
class DMult d1 d2 d3 | d1 d2 -> d3 where
 (..*) :: Unboxed -> d1 ->  d2 -> d3
 (../) :: Unboxed -> d1 -> d2 -> d3
 (..+) ::  Unboxed -> d1 -> d2 -> d3
 (..-) ::  Unboxed -> d1 -> d2 -> d3
 
instance DMult Val Val Val where
 (..*) _ x y = x*y
 (../) _  0 0 = 0
 (../) _ x y = x/y
 (..+) _ x y = x+y
 (..-) _ x y = x-y

instance DMult Val Bool Val where
 (..*) _ x True = x
 (..*) _ x False = 0
 (../) _ x False = 0
 (../) _ x True = x
 
instance DMult Bool Bool Bool where
-- And  
 (..*) _ x True = x
 (..*) _ x False = False
-- Or 
 (../) _ x False = x
 (../) _ x True = True

{-
class DSum d1 d2 d3 | d1 d2 -> d3 where
 ..+ ::  Unboxed -> d1 -> d2 -> d3
 ..- ::  Unboxed -> d1 -> d2 -> d3

instance DSum Val Val Val where
 ..+ _ x y = x+y
 ..- _ x y = x-y
-}

class DEq d1 d2 d3 | d1 d2 -> d3 where
  (..==) :: Unboxed -> d1 -> d2 -> d3
  (../=) :: Unboxed -> d1 -> d2 -> d3
  (..>=) :: Unboxed -> d1 -> d2 -> d3
  (..<=) :: Unboxed -> d1 -> d2 -> d3
  (..>) ::  Unboxed -> d1 -> d2 -> d3
  (..<) ::  Unboxed -> d1 -> d2 -> d3
  
instance DEq Val Val Bool where
  (..==) _ x y = x == y 
  (../=) _ x y = x /= y
  (..>=) _ x y = x >= y
  (..<=) _ x y = x <= y
  (..>) _  x y = x > y
  (..<) _  x y = x < y



sign :: Boxed -> Val -> Sign 
sign _ x | x == 0 = ZSign
sign _ x | x > 0 = PSign
sign _ x | x < 0 = NSign


----------------------------------------------------------
-- | 2. Data Structures with 
 
type Vec  = V.Vector -- deriving Show
type UVec = UV.Vector -- deriving Show
type Vec2 a = V.Vector (V.Vector a)
type UVec2 a = UV.Vector (UV.Vector a)

type List = [] -- deriving Show
type List2 a = [[a]] -- deriving Show
newtype DVal a = DVal a -- deriving Show
data DValPair a = DValPair a a 
newtype DSize a = DSize (a,[a]) 
  
-- Dimension Flags 
data D0 -- Skalar
data D1
data D2 -- horizontal
  
newtype DC dim c = DC c deriving Show

class DCCont s d where
    fromDC :: DC dim (s d) -> s d 
    toDC :: s d -> DC dim (s d)

class DValCont d where
    fromDVal :: DVal d -> d 
    toDVal :: d -> DVal d

-- class DApply dim s1 d
-- dapply :: DC dim a -> DC dim b
-- dapply f (DC x) =  x
   
-------------------------------------------------------------
-- | External Interface


dfmap :: SFunctor u s1 s2 d1 d2 => (u -> d1 -> d2) -> DC dim (s1 d1) -> DC dim (s2 d2)
dfmap f (DC x) = DC (smap f x)

dzipWith :: (SipWith u s1 s2 s3 d1 d2 d3) => (u -> d1 -> d2 -> d3) ->  DC dim1 (s1 d1) -> DC dim2 (s2 d2) -> DC dim3 (s3 d3)
dzipWith f (DC x) (DC y) = DC (sipWith f x y)

dzipWith2 :: (SipWith2 u c1 s1 s2 s3 d1 d2 d3) => (u -> d1 -> d2 -> d3) ->  DC dim1 (c1 (s1 d1)) -> DC dim2 (c1 (s2 d2)) -> DC dim3 (c1 (s3 d3))
dzipWith2 f (DC x) (DC y) = DC (sipWith2 f x y)

dzipWith02 :: (SipWith02 u c1 s1 s2 s3 d1 d2 d3) => (u -> d1 -> d2 -> d3) ->  DC dim1 (s1 d1) -> DC dim2 (c1 (s2 d2)) -> DC dim3 (c1 (s3 d3))
dzipWith02 f (DC x) (DC y) = DC (sipWith02 f x y)

dzipWith12 :: (SipWith12 u c1 s1 s2 s3 d1 d2 d3) => (u -> d1 -> d2 -> d3) ->  DC dim1 (s1 d1) -> DC dim2 (c1 (s2 d2)) -> DC dim3 (c1 (s3 d3))
dzipWith12 f (DC x) (DC y) = DC (sipWith12 f x y)

dfoldr ::  (SFold s d acc) => (d -> acc -> acc) -> DC D0 (DVal acc) ->  DC D1 (s d) -> DC D0 (DVal acc)
dfoldr f (DC acc) (DC x) = DC $ sfoldr f acc x 

dfoldl ::  (SFold s d acc) => (acc -> d -> acc)  -> DC D0 (DVal acc) ->  DC D1 (s d) -> DC D0 (DVal acc)
dfoldl f (DC acc) (DC x) = DC $ sfoldl f acc x 
  
-- dzipWith12' :: (SipWith12 u c1 s1 s2 s3 d1 d2 d3) => (u -> d1 -> d2 -> d3) ->  DC dim1 (s1 d1) -> DC dim2 (c1 (s2 d2)) -> DC dim3 (c1 (s3 d3))
-- dzipWith12' f (DC x) (DC y) = DC (sipWith12' f x y)

--------------------------------------------------------------
-- Max / min - 1dimensional

class DRange dim c d where
  getRange :: DC dim (c d) -> (DC D0 (DVal (d,d)))

instance  (SFold c d (d, d), Ord d,NeutralElement d) => DRange D1 c  d where
  getRange x =  dfoldl f (DC (DVal (neutral,neutral))) x
    where f (low, up) x = (min x low, max x up) 

-------------------------------------------------------------
-------------------------------------------------------------
-- Functor

-- One Dim
-- Own Functor class which could swap containers
class SFunctor u s1 s2 d1 d2 | u s1 -> s2, u s2 -> s1 where
  smap :: (u -> d1 -> d2) -> (s1 d1) -> (s2 d2) 

instance  SFunctor u DVal DVal d1 d2 where
  smap f (DVal x) = DVal $ (f undefined) x

instance (UV.Unbox d1, UV.Unbox d2) => SFunctor Unboxed UVec UVec d1 d2 where   
  smap f  x = UV.map (f undefined) x

instance  (UV.Unbox d2) => SFunctor Unboxed Vec UVec d1 d2  where   
  smap f x = V.convert $ V.map (f undefined) x 

instance  (UV.Unbox d1) => SFunctor Boxed UVec Vec d1 d2  where   
  smap f x = V.map (f undefined) $ UV.convert x

instance  SFunctor Boxed Vec Vec d1 d2  where   
  smap f x = V.map (f undefined) x  

instance  SFunctor u List List d1 d2 where   
  smap f x = map (f undefined) x

-------------------------------------------------------------
-- 2D

-- Own Deep Functor class which could swap containers
class SFunctor2 c1 u s1 d1 s2 d2 where
  smap2 :: (u -> d1 -> d2) -> (c1 (s1 d1)) -> (c1 (s2 d2)) 

instance SFunctor u s1 s2 d1 d2 => SFunctor2 Vec u s1 d1 s2 d2 where   
  smap2 f  x = V.map (smap f) x

instance SFunctor u s1 s2 d1 d2 => SFunctor2 List u s1 d1 s2 d2 where   
  smap2 f x = map (smap f ) x

-------------------------------------------------------------
-------------------------------------------------------------
-- fold   
class SFold s  d acc  where  
  sfoldl :: (acc -> d -> acc) ->  (DVal acc) -> (s d) ->  (DVal acc)
  sfoldr :: (d -> acc -> acc) ->  (DVal acc) -> (s d) ->  (DVal acc)

instance SFold List  d acc where
  sfoldl f (DVal acc) x = DVal $ L.foldl f acc x    
  sfoldr f (DVal acc) x = DVal $ foldr f acc x    

instance SFold Vec  d acc where
  sfoldl f (DVal acc) x = DVal $ V.foldl f acc x    
  sfoldr f (DVal acc) x = DVal $ V.foldr f acc x    

instance UV.Unbox d => SFold UVec  d acc where
  sfoldl f (DVal acc) x = DVal $ UV.foldl f acc x    
  sfoldr f (DVal acc) x = DVal $ UV.foldr f acc x    

dlen (DC x) = len x 

-------------------------------------------------------------
-------------------------------------------------------------
-- Zip

m1 = "Error in DZipWith -- unequal length"

-- Zip Class
class SipWith u s1 s2 s3 d1 d2 d3  | u s1 s2 -> s3, d1 d2 -> d3  where 
  sipWith :: (u -> d1 -> d2 -> d3) -> (s1 d1) -> (s2 d2) -> (s3 d3) 

-- Val to one dimensional zip
instance (UV.Unbox d3, GetLength (s2 d2), SFunctor Unboxed s2 s3 d2 d3) => SipWith Unboxed DVal s2 s3 d1 d2 d3 where
  sipWith f x@(DVal x') y = if lCheck x y then smap ((flip f) x') y else error m1 
  
instance (GetLength (s2 d2), SFunctor Boxed s2 s3 d2 d3) => SipWith Boxed DVal s2 s3 d1 d2 d3 where
  sipWith f x@(DVal x') y = if lCheck x y then smap ((flip f) x') y else error m1 

-- one Dimensional zip
instance (UV.Unbox d1, UV.Unbox d2, UV.Unbox d3) => SipWith Unboxed UVec UVec UVec d1 d2 d3 where
  sipWith f x y = if lCheck x y then UV.zipWith (f undefined) x y else error m1 

instance (UV.Unbox d1, UV.Unbox d2) => SipWith Boxed UVec UVec Vec d1 d2 d3  where
  sipWith f x y = if lCheck x y then  V.zipWith (f undefined) (V.convert x) (V.convert y) else error m1  

instance (UV.Unbox d1) => SipWith Boxed UVec Vec Vec d1 d2 d3  where
  sipWith f x y = if lCheck x y then  V.zipWith (f undefined) (V.convert x) y  else error m1  

instance (UV.Unbox d2) => SipWith Boxed Vec UVec Vec d1 d2 d3  where
  sipWith f x y = if lCheck x y then  V.zipWith (f undefined) x (V.convert y)else error m1   

instance (UV.Unbox d3) => SipWith Unboxed Vec Vec UVec d1 d2 d3  where
  sipWith f x y = if lCheck x y then  UV.convert $ V.zipWith (f undefined) x y  else error m1

instance (UV.Unbox d1,UV.Unbox d3) => SipWith Unboxed UVec Vec UVec d1 d2 d3  where
  sipWith f x y = if lCheck x y then  UV.convert $ V.zipWith (f undefined) (V.convert x) y  else error m1

instance (UV.Unbox d2,UV.Unbox d3) => SipWith Unboxed Vec UVec UVec d1 d2 d3  where
  sipWith f x y = if lCheck x y then  UV.convert $ V.zipWith (f undefined) x (V.convert y)  else error m1

instance SipWith Boxed Vec Vec Vec d1 d2 d3  where
  sipWith f x y =  if lCheck x y then  V.zipWith (f undefined) x y else error m1 

instance SipWith Boxed List List List d1 d2 d3  where
  sipWith f x y = if lCheck x y then  zipWith (f undefined) x y  else error m1

---------------------------------------------------------------
-- ZipWith2

-- Zip Class
class SipWith2 u c1 s1 s2 s3 d1 d2 d3 where
  sipWith2 :: (u -> d1 -> d2 -> d3) -> (c1 (s1 d1)) -> (c1 (s2 d2)) -> (c1 (s3 d3)) 

instance  SipWith u s1 s2 s3 d1 d2 d3 => SipWith2 u Vec s1 s2 s3 d1 d2 d3  where
  sipWith2 f x y = if lCheck x y then V.zipWith (sipWith f) x y else error m1 

instance  SipWith u s1 s2 s3 d1 d2 d3 => SipWith2 u List s1 s2 s3 d1 d2 d3  where
  sipWith2 f x y = if lCheck x y then zipWith (sipWith f) x y else error m1 

---------------------------------------------------------------
-- ZipWith02

-- Zip Class
class SipWith02 u c1 s1 s2 s3 d1 d2 d3 where
  sipWith02 :: (u -> d1 -> d2 -> d3) -> (s1 d1) -> (c1 (s2 d2)) -> (c1 (s3 d3)) 

instance  (SFunctor u s2 s3 d2 d3) => SipWith02 u Vec DVal s2 s3 d1 d2 d3  where
  sipWith02 f x@(DVal x') y = smap2 ((flip f) x') y 

instance   (SFunctor u s2 s3 d2 d3) => SipWith02 u List DVal s2 s3 d1 d2 d3  where
  sipWith02 f x@(DVal x') y = smap2 ((flip f) x') y 

-- Zip Class
class SipWith12 u c1 s1 s2 s3 d1 d2 d3 where
  sipWith12 :: (u -> d1 -> d2 -> d3) -> (s1 d1) -> (c1 (s2 d2)) -> (c1 (s3 d3)) 

instance SipWith u s1 s2 s3 d1 d2 d3 => SipWith12 u Vec s1 s2 s3 d1 d2 d3  where
  sipWith12 f x y = V.map (sipWith f x) y 

instance SipWith u s1 s2 s3 d1 d2 d3 => SipWith12 u List s1 s2 s3 d1 d2 d3  where
  sipWith12 f x y = map (sipWith f x) y

{-
-- Zip12
class SipWith12' u c1 s1 s2 s3 d1 d2 d3 where
  sipWith12' :: (u -> d1 -> d2 -> d3) -> (s1 d1) -> (c1 (s2 d2)) -> (c1 (s3 d3)) 

instance SipWith u DVal c1 c1 d1 d2 d3 => SipWith12' u c1 s1 s2 s2 d1 d2 d3  where
  sipWith12' f x y = sipWith f' x y 
    where f' x' y' = smap (f (DVal x')) y'
-}



---------------------------------------------------------------
-- Monoid
class DMonoid c d where
 eempty :: (c d)
 (.++) :: (c d) -> (c d) -> (c d)
 
instance DMonoid List d where 
  eempty = [] 
  (.++) = (++)

instance DMonoid Vec d where 
  eempty = V.empty
  (.++) = (V.++)

instance UV.Unbox d => DMonoid UVec d where 
  eempty = UV.empty
  (.++) = (UV.++)

--------------------------------------------------------------
-- Vector conversion
  
class DConvert a b where   
  dconvert :: DC dim a -> DC dim b
  
-- One D
instance (UV.Unbox d) => DConvert (Vec d) (UVec d) where
  dconvert (DC x) = DC $ V.convert x 

instance (UV.Unbox d) => DConvert (UVec d) (Vec d) where
  dconvert (DC x) = DC $ UV.convert x 

instance DConvert (Vec d) (List d) where
  dconvert (DC x) = DC $ V.toList x

instance DConvert (List d) (Vec d) where
  dconvert (DC x) = DC $ V.fromList x
  
instance (UV.Unbox d) => DConvert (List d) (UVec d) where
  dconvert (DC x) = DC $ UV.fromList x

instance (UV.Unbox d) => DConvert (UVec d) (List d) where
  dconvert (DC x) = DC $ UV.toList x

--------------------------------------------------------------
-- Length & Length Check

class GetLength s where
  len :: (s) -> Int

instance GetLength  (DVal d) where 
  len  x = 1

instance GetLength  (Vec d) where 
  len x =  V.length x

instance UV.Unbox d => GetLength  (UVec d) where 
  len x = UV.length x

instance GetLength  (List d) where 
  len x = length x

lCheck x y = len x == len y




-- Two D
-- TODO   

-- instance HArith h1 h2 h3 => HArith h2 h1 h3 
class DArith1 e1 e2 e3 | e1 e2 -> e3 where
   (.*) :: e1 -> e2 -> e3
   (./) :: e1 -> e2 -> e3
   (.+) :: e1 -> e2 -> e3
   (.-) :: e1 -> e2 -> e3

instance (SipWith Unboxed s1 s2 s3 d1 d2 d3, DMult d1 d2 d3) => DArith1 (DC D0 (s1 d1)) (DC D0 (s2 d2)) (DC D0 (s3 d3)) where
  (.*) x y =  dzipWith (..*) x y
  (./) x y = dzipWith (../) x y
  (.+) x y = dzipWith (..+) x y
  (.-) x y = dzipWith (..-) x y

instance (SipWith Unboxed s1 s2 s3 d1 d2 d3, DMult d1 d2 d3) => DArith1 (DC D0 (s1 d1)) (DC D1 (s2 d2)) (DC D1 (s3 d3)) where
  (.*) x y =  dzipWith (..*) x y
  (./) x y = dzipWith (../) x y
  (.+) x y = dzipWith (..+) x y
  (.-) x y = dzipWith (..-) x y

instance (SipWith Unboxed s1 s2 s3 d1 d2 d3, DMult d1 d2 d3) => DArith1 (DC D1 (s1 d1)) (DC D1 (s2 d2)) (DC D1 (s3 d3)) where
   (.*) x y =  dzipWith (..*) x y
   (./) x y = dzipWith (../) x y
   (.+) x y = dzipWith (..+) x y
   (.-) x y = dzipWith (..-) x y

instance (SipWith2 Unboxed c1 s1 s2 s3 d1 d2 d3, DMult d1 d2 d3) => DArith1 (DC D2 (c1 (s1 d1))) (DC D2 (c1(s2 d2))) (DC D2 (c1(s3 d3))) where
  (.*) x y =  dzipWith2 (..*) x y
  (./) x y = dzipWith2 (../) x y
  (.+) x y = dzipWith2 (..+) x y
  (.-) x y = dzipWith2 (..-) x y

instance (SipWith02 Unboxed c1 s1 s2 s3 d1 d2 d3, DMult d1 d2 d3) => DArith1 (DC D0 (s1 d1)) (DC D2 (c1(s2 d2))) (DC D2 (c1(s3 d3))) where
  (.*) x y = dzipWith02 (..*) x y
  (./) x y = dzipWith02 (../) x y
  (.+) x y = dzipWith02 (..+) x y
  (.-) x y = dzipWith02 (..-) x y

instance (SipWith12 Unboxed c1 s1 s2 s3 d1 d2 d3, DMult d1 d2 d3) => DArith1 (DC D1 (s1 d1)) (DC D2 (c1(s2 d2))) (DC D2 (c1(s3 d3))) where
  (.*) x y = dzipWith12 (..*) x y
  (./) x y = dzipWith12 (../) x y
  (.+) x y = dzipWith12 (..+) x y
  (.-) x y = dzipWith12 (..-) x y


{-
class  DCSum e1 e2 e3 where
  (.+) :: e1 -> e2 -> e3
  (.-) :: e1 -> e2 -> e3

instance (SipWith Unboxed s1 s2 s3 d1 d2 d3, DSum d1 d2 d3) => DCSum (DC h (s1 d1)) (DC h (s2 d2)) (DC h (s3 d3)) where
  (.+) x y = dzipWith (..+) x y
  (.-) x y = dzipWith (..-) x y
-}
{-
class DCEq e1 e2 e3 where
  (.==) :: e1 -> e2 -> e3
  (./=) :: e1 -> e2 -> e3 
  (.>=) :: e1 -> e2 -> e3 
  (.<=) :: e1 -> e2 -> e3
  (.>)  :: e1 -> e2 -> e3
  (.<)  :: e1 -> e2 -> e3
  
instance (SipWith Unboxed s1 s2 s3 d1 d2 d3, DEq d1 d2 d3) => DCEq (DC h (s1 d1)) (DC h (s2 d2)) (DC h (s3 d3)) where
  (.==)  x y = dzipWith (..==) x y
  (./=)  x y = dzipWith (../=) x y
  (.>=)  x y = dzipWith (..>=) x y
  (.<=)  x y = dzipWith (..<=) x y
  (.>)  x y = dzipWith (..>) x y
  (.<)  x y = dzipWith (..<) x y
-}



