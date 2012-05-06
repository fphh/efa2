{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, GeneralizedNewtypeDeriving,FlexibleContexts,OverlappingInstances #-} 

module EFA2.Signal.Vector2 (module EFA2.Signal.Vector2) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV

import Data.Monoid
import Control.Applicative
import EFA2.Utils.Utils

----------------------------------------------------------
-- | 1. Numeric Data types
type Val = Double -- or Ratio
type IState = Int
type BState = Bool 

-- Phantom Type Flag to mark functions
data Unboxed
data Boxed


-- | Calculation classes for basic Datatypes
class DMult d1 d2 d3 | d1 d2 -> d3 where
 dmult :: Unboxed -> d1 ->  d2 -> d3
 ddiv :: Unboxed -> d1 -> d2 -> d3
 
instance DMult Val Val Val where
 dmult _ x y = x*y
 ddiv _  0 0 = 0
 ddiv _ x y = x/y

instance DMult Val Bool Val where
 dmult _ x True = x
 dmult _ x False = 0
 ddiv _ x False = 0
 ddiv _ x True = x
 
instance DMult Bool Bool Bool where
-- And  
 dmult _ x True = x
 dmult _ x False = False
-- Or 
 ddiv _ x False = x
 ddiv _ x True = True

class DSum d1 d2 d3 | d1 d2 -> d3 where
 dadd ::  Unboxed -> d1 -> d2 -> d3
 dsub ::  Unboxed -> d1 -> d2 -> d3

instance DSum Val Val Val where
 dadd _ x y = x+y
 dsub _ x y = x-y

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

data Sign = PSign | ZSign | NSign deriving Show

sign :: Boxed -> Val -> Sign 
sign _ x | x == 0 = ZSign
sign _ x | x > 0 = PSign
sign _ x | x < 0 = NSign


----------------------------------------------------------
-- | 2. Data Structures with 
 
type Vec  = V.Vector -- deriving Show
type UVec = UV.Vector -- deriving Show

type List = [] -- deriving Show
newtype DVal a = DVal a -- deriving Show

-- Dimension Flags 
data S -- Skalar
data H
data H2 -- horizontal
data V -- Vertical
data V2  
  
newtype DC dim c = DC c deriving Show

class DCCont w s where
   toCont :: w sim s d -> s d 
   fromCont :: s d -> w h s d
   

dfmap :: SFunctor u s1 s2 d1 d2 => (u -> d1 -> d2) -> DC dim (s1 d1) -> DC dim (s2 d2)
dfmap f (DC x) = DC (smap f x)

dzipWith :: (SipWith u s1 s2 s3 d1 d2 d3) => (u -> d1 -> d2 -> d3) ->  DC dim1 (s1 d1) -> DC dim2 (s2 d2) -> DC dim3 (s3 d3)
dzipWith f (DC x) (DC y) = DC (sipWith f x y)

dzipWith2 :: (SipWith u s1 s2 s3 d1 d2 d3) => (u -> d1 -> d2 -> d3) ->  DC dim1 (s1 d1) -> DC dim2 (s2 d2) -> DC dim3 (s3 d3)
dzipWith2 f (DC x) (DC y) = DC (sipWith f x y)

{-
dzipWith0 :: (SipWith u s1 s2 s3 d1 d2 d3) => (u -> d1 -> d2 -> d3) ->  DC dim1 (s1 d1) -> DC dim2 (s2 d2) -> DC dim3 (s3 d3)
dzipWith0 f (DC x) (DC y) = DC (sipWith f x y)
-}
-------------------------------------------------------------
-- Functor

-- Own Functor class which could swap containers
class SFunctor u s1 s2 d1 d2 | u s1 -> s2  where
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
-- Functor

-- Own Deep Functor class which could swap containers
class SFunctor2 c1 u s1 d1 s2 d2 where
  smap2 :: (u -> d1 -> d2) -> (c1 (s1 d1)) -> (c1 (s2 d2)) 

instance SFunctor u s1 s2 d1 d2 => SFunctor2 Vec u s1 d1 s2 d2 where   
  smap2 f  x = V.map (smap f) x

instance SFunctor u s1 s2 d1 d2 => SFunctor2 List u s1 d1 s2 d2 where   
  smap2 f x = map (smap f ) x
  

m1 = "Error in DZipWith -- unequal length"

-- Zip Class
class SipWith u s1 s2 s3 d1 d2 d3  | u s1 s2 -> s3 where 
  sipWith :: (u -> d1 -> d2 -> d3) -> (s1 d1) -> (s2 d2) -> (s3 d3) 

-- one Dimensional zip
instance (UV.Unbox d1, UV.Unbox d2, UV.Unbox d3, GetLength (s2 d2), SFunctor Unboxed s2 s3 d2 d3) => SipWith Unboxed DVal s2 s3 d1 d2 d3 where
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
-- ZipWith

-- Zip Class
class SipWith2 c1 u s1 s2 s3 d1 d2 d3 where
  sipWith2 :: (u -> d1 -> d2 -> d3) -> (c1 (s1 d1)) -> (c1 (s2 d2)) -> (c1 (s3 d3)) 

instance  SipWith u s1 s2 s3 d1 d2 d3 => SipWith2 Vec u s1 s2 s3 d1 d2 d3  where
  sipWith2 f x y = if lCheck x y then V.zipWith (sipWith f) x y else error m1 

instance  SipWith u s1 s2 s3 d1 d2 d3 => SipWith2 List u s1 s2 s3 d1 d2 d3  where
  sipWith2 f x y = if lCheck x y then zipWith (sipWith f) x y else error m1 

---------------------------------------------------------------
-- ZipWith
{-
-- Zip Class
class SipWith01 c1 u s1 s2 s3 d1 d2 d3 where
  sipWith01 :: (u -> d1 -> d2 -> d3) -> (s1 d1) -> (c1 (s2 d2)) -> (c1 (s3 d3)) 

instance  SipWith u s1 s2 s3 d1 d2 d3 => SipWith0 Vec u s1 s2 s3 d1 d2 d3  where
  sipWith01 f x y = if lCheck x y then V.zipWith (sipWith f) x y else error m1 
-}


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

{-
---------------------------------------------------------------
-- Monoid

class EMonoid s1 s2 d where
  eempty :: s2 d 
  (.++) :: s1 d -> s2 d -> s2 d
    
instance EMonoid EList EList d where 
  eempty = EList [] 
  (.++) (EList x) (EList y) = EList (x++y)  

instance EMonoid Vec Vec d where 
  eempty = Vec $ GV.fromList [] 
  (.++) (Vec x) (Vec y) = Vec (x GV.++ y)  

instance UV.Unbox d => EMonoid UVec UVec d where 
  eempty = UVec $ UV.fromList [] 
  (.++) (UVec x) (UVec y) = UVec (x UV.++ y)  

  
---------------------------------------------------------------
-- Vector Packing 
  
class EC s s d where
  toEC :: (s d) -> (c d)  
  fromEC:: (c d) -> (s d)

instance EC EList Lst d where 
  toEC x = EList x  
  fromEC (EList x) = x
  
instance EC (Vec d) Vec d where 
  toEC x = Vec x  
  fromEC (Vec x) = x

instance EC UVec UVec d where 
  toEC x = UVec x  
  fromEC (UVec x) = x

instance EC EList2 Lst2 d where 
  toEC x = EList2 x  
  fromEC (EList2 x) = x
  
instance EC Vec2 Vec2 d where 
  toEC x = Vec2 x  
  fromEC (Vec2 x) = x

instance EC UVec2 UVec2 d where 
  toEC x = UVec2 x  
  fromEC (UVec2 x) = x

--------------------------------------------------------------
-- Vector sonversion
  
class EConvert s1 s2 d where   
  econvert :: s1 d -> s2 d
  
-- One D
instance (UV.Unbox d) => EConvert Vec UVec d where
   econvert (Vec x) = UVec $ V.convert x 

instance (UV.Unbox d) => EConvert UVec Vec d where
   econvert (UVec x) = Vec $ UV.convert x 

instance EConvert Vec EList d where
  econvert (Vec x)  = EList $ V.toList x 

instance  (UV.Unbox d) => EConvert UVec EList d where
  econvert (UVec x)  = EList $ UV.toList x
  
instance EConvert EList Vec d where
  econvert (EList x)  = Vec $ V.fromList x 

instance  (UV.Unbox d) => EConvert EList UVec d where
  econvert (EList x)  = UVec $ UV.fromList x

-- Two D
instance (UV.Unbox d) => EConvert Vec2 UVec2 d where
   econvert (Vec2 x) = UVec2 $ V.map V.convert x 

instance (UV.Unbox d) => EConvert UVec2 Vec2 d where
   econvert (UVec2 x) = Vec2 $ V.map UV.convert x 

instance EConvert Vec2 EList2 d where
  econvert (Vec2 x)  = EList2 $ V.toList $ V.map V.toList x 

instance  (UV.Unbox d) => EConvert UVec2 EList2 d where
  econvert (UVec2 x)  = EList2 $ V.toList $ V.map UV.toList x
  
instance EConvert EList2 Vec2 d where
  econvert (EList2 x)  = Vec2 $ V.fromList $ map V.fromList x 

instance  (UV.Unbox d) => EConvert EList2 UVec2 d where
  econvert (EList2 x)  = UVec2 $ V.fromList $ map UV.fromList x
-}


---------------------------------------------------------------
-- Arith Funktionen  

class Hzip h1 h2 h3 | h1 h2 -> h3
instance Hzip D0 D1 D1
instance Hzip D0 D2 D2
instance Hzip D1 D1 D1
instance Hzip12 D2 D2 D2 
instance Hzip12 D1 D2 D2 
-- aufspannen
-- unten rein / oben drauf

instance S H H
instance S V V

instance H H H
instance V V V 

instance H H H
instance V V V 


-- instance HArith h1 h2 h3 => HArith h2 h1 h3 


class DCMult e1 e2 e3 | e1 e2 -> e3 where
  (.*) :: e1 -> e2 -> e3
  (./) :: e1 -> e2 -> e3
  
instance (SipWith Unboxed s1 s2 s3 d1 d2 d3, DMult d1 d2 d3, Hzip h1 h2 h3) => DCMult (DC h1 (s1 d1)) (DC h2 (s2 d2)) (DC h3 (s3 d3)) where
  (.*) x y =  dzipWith dmult x y
  (./) x y = dzipWith ddiv x y

instance (SipWith Unboxed s1 s2 s3 d1 d2 d3, DMult d1 d2 d3, Hzip2 h1 h2 h3) => DCMult (DC h1 (s1 d1)) (DC h2 (s2 d2)) (DC h3 (s3 d3)) where
  (.*) x y =  dzipWith2 dmult x y
  (./) x y = dzipWith2 ddiv x y




class  DCSum e1 e2 e3 where
  (.+) :: e1 -> e2 -> e3
  (.-) :: e1 -> e2 -> e3

instance (SipWith Unboxed s1 s2 s3 d1 d2 d3, DSum d1 d2 d3) => DCSum (DC h (s1 d1)) (DC h (s2 d2)) (DC h (s3 d3)) where
  (.+) x y = dzipWith (dadd) x y
  (.-) x y = dzipWith (dsub) x y

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




