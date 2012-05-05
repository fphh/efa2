{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, GeneralizedNewtypeDeriving,FlexibleContexts,OverlappingInstances #-} 

module EFA2.Signal.Vector (module EFA2.Signal.Vector) where

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
 
newtype Vec a  = Vec (V.Vector a) deriving Show
newtype UVec a = UVec (UV.Vector a) deriving Show
newtype UVec2 a = UVec2 (V.Vector (UV.Vector a)) deriving Show
newtype Vec2 a = Vec2 (V.Vector (V.Vector a)) deriving Show
newtype List a = List [a] deriving Show
newtype List2 a = List2 [[a]] deriving Show
newtype DVal a = DVal a deriving Show

-- Dimension Flags 
data D1 -- horizontal
data D2 -- Vertical
data D3 -- None
-- data H2 -- Horizontal 2D
-- data V2 -- Vertical 2D
  
newtype DC h c = DC c deriving Show



class Cont w s where
  toCont :: w h s d -> s d 
  fromCont :: s d -> w h s d

-------------------------------------------------------------
-- Functor

-- Own Functor class which could swap containers
class DFunctor u s1 s2 d1 d2 | u s1 -> s2  where
  dfmap :: (u -> d1 -> d2) -> DC h (s1 d1) -> DC h (s2 d2) 

-- 0 dim
instance  DFunctor u DVal DVal d1 d2 where
  dfmap f (DC (DVal x)) = DC $ DVal $ (f undefined) x

-- 1 dim
instance (UV.Unbox d1, UV.Unbox d2) => DFunctor Unboxed UVec UVec d1 d2 where   
  dfmap f (DC (UVec x)) = DC $ UVec $ UV.map (f undefined) x

instance  (UV.Unbox d2) => DFunctor Unboxed Vec UVec d1 d2  where   
  dfmap f (DC (Vec x)) = DC $ UVec $ V.convert $ V.map (f undefined) x 

instance  (UV.Unbox d1) => DFunctor Boxed UVec Vec d1 d2  where   
   dfmap f (DC (UVec x)) = DC $ Vec $ V.map (f undefined) $ UV.convert x

instance  DFunctor Boxed Vec Vec d1 d2  where   
   dfmap f (DC (Vec x)) = DC $ Vec $ V.map (f undefined) x  

instance  DFunctor u List List d1 d2  where   
  dfmap f (DC (List x)) = DC $ List $  map (f undefined) x

-- 2 dim  
instance (UV.Unbox d1, UV.Unbox d2) => DFunctor Unboxed UVec2 UVec2 d1 d2  where   
  dfmap f (DC (UVec2 x)) = DC $ UVec2 $ V.map (UV.map (f undefined)) x

instance  (UV.Unbox d2) => DFunctor Unboxed Vec2  UVec2  d1 d2  where   
  dfmap f (DC (Vec2 x)) = DC $ UVec2 $ V.map UV.convert $ V.map (V.map (f undefined)) x

instance  (UV.Unbox d1) => DFunctor Boxed UVec2 Vec2  d1 d2  where   
   dfmap f (DC (UVec2 x)) = DC $ Vec2 $ V.map (V.map (f undefined)) $ V.map UV.convert x

instance  DFunctor Boxed Vec2  Vec2  d1 d2  where   
   dfmap f (DC (Vec2 x)) = DC $ Vec2 $ V.map (V.map (f undefined)) x

instance  DFunctor u List2  List2  d1 d2  where   
  dfmap f (DC (List2 x)) = DC $ List2 $ map (map (f undefined)) x

 
---------------------------------------------------------------
-- ZipWith

-- Zip Class
class DZipWith u s1 s2 s3 d1 d2 d3  | u s1 s2 -> s3 where 
  dzipWith :: (u -> d1 -> d2 -> d3) -> DC h (s1 d1) -> DC h (s2 d2) -> DC h (s3 d3) 

-- EValue against all
instance  (DFunctor u s2 s3 d2 d3) => DZipWith u DVal s2 s3 d1 d2 d3  where
  dzipWith f (DC (DVal x)) y = dfmap ((flip f) x) y  

-- one Dimensional zip
instance (UV.Unbox d1, UV.Unbox d2, UV.Unbox d3) => DZipWith Unboxed UVec UVec UVec d1 d2 d3 where
  dzipWith f u@(DC (UVec x)) v@(DC (UVec y)) = if lCheck u v then DC $ UVec $ (UV.zipWith (f undefined)) x y else error m1 

m1 = "Error in DZipWith -- unequal length"

instance (UV.Unbox d1, UV.Unbox d2) => DZipWith Boxed UVec UVec Vec d1 d2 d3  where
  dzipWith f u@(DC (UVec x)) v@(DC (UVec y)) = if lCheck u v then DC $ Vec $ V.zipWith (f undefined) (V.convert x) (V.convert y) else error m1  

instance (UV.Unbox d1) => DZipWith Boxed UVec Vec Vec d1 d2 d3  where
  dzipWith f u@(DC (UVec x)) v@(DC (Vec y)) = if lCheck u v then DC $ Vec $ V.zipWith (f undefined) (V.convert x) y  else error m1  

instance (UV.Unbox d2) => DZipWith Boxed Vec UVec Vec d1 d2 d3  where
  dzipWith f u@(DC (Vec x)) v@(DC (UVec y)) = if lCheck u v then DC $ Vec $ V.zipWith (f undefined) x (V.convert y)else error m1   

instance (UV.Unbox d3) => DZipWith Unboxed Vec Vec UVec d1 d2 d3  where
  dzipWith f u@(DC (Vec x)) v@(DC (Vec y)) = if lCheck u v then DC $ UVec $ UV.convert $ V.zipWith (f undefined) x y  else error m1

instance (UV.Unbox d1,UV.Unbox d3) => DZipWith Unboxed UVec Vec UVec d1 d2 d3  where
  dzipWith f u@(DC (UVec x)) v@(DC (Vec y)) = if lCheck u v then DC $ UVec $ UV.convert $ V.zipWith (f undefined) (V.convert x) y  else error m1

instance (UV.Unbox d2,UV.Unbox d3) => DZipWith Unboxed Vec UVec UVec d1 d2 d3  where
  dzipWith f u@(DC (Vec x)) v@(DC (UVec y)) = if lCheck u v then DC $ UVec $ UV.convert $ V.zipWith (f undefined) x (V.convert y)  else error m1

instance DZipWith Boxed Vec Vec Vec d1 d2 d3  where
  dzipWith f u@(DC (Vec x)) v@(DC (Vec y)) =  if lCheck u v then DC $ Vec $ V.zipWith (f undefined) x y else error m1 

instance DZipWith Boxed List List List d1 d2 d3  where
  dzipWith f u@(DC (List x)) v@(DC(List y)) = if lCheck u v then DC $ List $ zipWith (f undefined) x y  else error m1

-- two Dimensional zip
instance (UV.Unbox d1, UV.Unbox d2, UV.Unbox d3) => DZipWith Unboxed UVec2 UVec2 UVec2 d1 d2 d3  where
  dzipWith f u@(DC(UVec2 x)) v@(DC(UVec2 y)) = if lCheck u v then DC $ UVec2 $ V.zipWith (UV.zipWith (f undefined)) x y else error m1 

instance (UV.Unbox d1, UV.Unbox d2) => DZipWith Boxed UVec2 UVec2 Vec2 d1 d2 d3  where
  dzipWith f u@(DC(UVec2 x)) v@(DC(UVec2 y)) = if lCheck u v then DC $ Vec2 $ V.zipWith (V.zipWith (f undefined)) (V.map V.convert x) (V.map V.convert y) else error m1  

instance (UV.Unbox d1) => DZipWith Boxed UVec2 Vec2 Vec2 d1 d2 d3  where
  dzipWith f u@(DC(UVec2 x)) v@(DC(Vec2 y)) = if lCheck u v then DC $ Vec2 $ V.zipWith (V.zipWith (f undefined)) (V.map V.convert x) y  else error m1  

instance (UV.Unbox d2) => DZipWith Boxed Vec2 UVec2 Vec2 d1 d2 d3  where
  dzipWith f u@(DC(Vec2 x)) v@(DC(UVec2 y)) = if lCheck u v then DC $ Vec2 $ V.zipWith (V.zipWith (f undefined)) x (V.map V.convert y)else error m1   

instance (UV.Unbox d3) => DZipWith Unboxed Vec2 Vec2 UVec2 d1 d2 d3  where
  dzipWith f u@(DC(Vec2 x)) v@(DC(Vec2 y)) = if lCheck u v then DC $ UVec2 $ V.map UV.convert $ V.zipWith (V.zipWith (f undefined)) x y  else error m1

instance (UV.Unbox d1,UV.Unbox d3) => DZipWith Unboxed UVec2 Vec2 UVec2 d1 d2 d3  where
  dzipWith f u@(DC(UVec2 x)) v@(DC(Vec2 y)) = if lCheck u v then DC $ UVec2 $ V.map UV.convert $ V.zipWith (V.zipWith (f undefined)) (V.map V.convert x) y  else error m1

instance (UV.Unbox d2,UV.Unbox d3) => DZipWith Unboxed Vec2 UVec2 UVec2 d1 d2 d3  where
  dzipWith f u@(DC(Vec2 x)) v@(DC(UVec2 y)) = if lCheck u v then DC $ UVec2 $ V.map UV.convert $ V.zipWith (V.zipWith (f undefined)) x (V.map V.convert y)  else error m1

instance DZipWith Boxed Vec2 Vec2 Vec2 d1 d2 d3  where
  dzipWith f u@(DC(Vec2 x)) v@(DC(Vec2 y)) =  if lCheck u v then DC $ Vec2 $ V.zipWith (V.zipWith (f undefined)) x y else error m1 

instance DZipWith Boxed List2 List2 List2 d1 d2 d3  where
  dzipWith f u@(DC(List2 x)) v@(DC(List2 y)) = if lCheck u v then DC $ List2 $ zipWith (zipWith (f undefined)) x y  else error m1

----------------------------------------------------------------
-- HV zipWith  

-- -- Zip Class
-- class HVZipWith u s1 s2 s3 d1 d2 d3 | u s1 s2 -> s3 where 
--   hvzipWith :: (u -> d1 -> d2 -> d3) -> DC h (s1 d1) -> DC h (s2 d2) -> DC h (s3 d3) 
  
-- instance (UV.Unbox d1, UV.Unbox d2, UV.Unbox d3) => HVZipWith Unboxed UVec UVec UVec2 d1 d2 d3 where
--   hvzipWith f u@(DC (UVec x)) v@(DC (UVec y)) = if lCheck u v then DC $ UVec2 $ V.map (UV.map (f undefined x)) $ V.replicate (UV.length x) (V.fromList [y]) else error m1 

 

---------------------------------------------------------------
-- Length & Length Check

class GetLength s where
  len :: (DC h s) -> (Int,[Int])

instance GetLength  (DVal d) where 
  len (DC (DVal x)) = (1,[])

instance GetLength  (Vec d) where 
  len (DC (Vec x)) =  (V.length x,[])

instance UV.Unbox d => GetLength  (UVec d) where 
   len (DC (UVec x)) = (UV.length x,[])

instance GetLength  (List d) where 
   len (DC (List x)) = (length x,[])

instance GetLength  (Vec2 d) where 
  len (DC ( Vec2 x)) = (V.length x,V.toList $ V.map V.length x)

instance UV.Unbox d => GetLength  (UVec2 d) where 
   len (DC (UVec2 x)) = (V.length x, V.toList $ V.map UV.length x)

instance GetLength  (List2 d)  where 
   len (DC (List2 x)) = (length x,map length x)
   

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

class DCMult e1 e2 e3 | e1 e2 -> e3 where
  (.*) :: e1 -> e2 -> e3
  (./) :: e1 -> e2 -> e3
  
instance (DZipWith Unboxed s1 s2 s3 d1 d2 d3, DMult d1 d2 d3) => DCMult (DC h (s1 d1)) (DC h (s2 d2)) (DC h (s3 d3)) where
  (.*) x y =  dzipWith dmult x y
  (./) x y = dzipWith ddiv x y
  
class  DCSum e1 e2 e3 where
  (.+) :: e1 -> e2 -> e3
  (.-) :: e1 -> e2 -> e3

instance (DZipWith Unboxed s1 s2 s3 d1 d2 d3, DSum d1 d2 d3) => DCSum (DC h (s1 d1)) (DC h (s2 d2)) (DC h (s3 d3)) where
  (.+) x y = dzipWith (dadd) x y
  (.-) x y = dzipWith (dsub) x y

class DCEq e1 e2 e3 where
  (.==) :: e1 -> e2 -> e3
  (./=) :: e1 -> e2 -> e3 
  (.>=) :: e1 -> e2 -> e3 
  (.<=) :: e1 -> e2 -> e3
  (.>)  :: e1 -> e2 -> e3
  (.<)  :: e1 -> e2 -> e3
  
instance (DZipWith Unboxed s1 s2 s3 d1 d2 d3, DEq d1 d2 d3) => DCEq (DC h (s1 d1)) (DC h (s2 d2)) (DC h (s3 d3)) where
  (.==)  x y = dzipWith (..==) x y
  (./=)  x y = dzipWith (../=) x y
  (.>=)  x y = dzipWith (..>=) x y
  (.<=)  x y = dzipWith (..<=) x y
  (.>)  x y = dzipWith (..>) x y
  (.<)  x y = dzipWith (..<) x y




