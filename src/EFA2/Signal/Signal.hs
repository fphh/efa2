{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, TypeOperators,  GADTs, OverlappingInstances, FlexibleContexts, ScopedTypeVariables #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where
import EFA2.Signal.Vector
import EFA2.Signal.Base
import EFA2.Signal.Data
import EFA2.Signal.Typ
-- import EFA2.Signal.Vector
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Monoid
import qualified Data.List as L
----------------------------------------------------------
-- | Signal & Company

newtype TC s t d = TC d  deriving (Show, Eq, Ord)

data Scalar

data Signal
data Sample
  
data FSignal
data FSample

data FDistrib
data FClass

data TestRow
  
----------------------------------------------------------------
-- Signal Zipwith with Rule of Signal Inheritance
class SArith s1 s2 s3 | s1 s2 -> s3

instance SArith Scalar Scalar Scalar

instance SArith Scalar Signal Signal
instance SArith Scalar Sample Sample
instance SArith Scalar FSignal FSignal
instance SArith Scalar FSample FSample
instance SArith Scalar FDistrib FDistrib
instance SArith Scalar FClass FClass

instance SArith Signal Scalar Signal
instance SArith Sample Scalar Sample
instance SArith FSignal Scalar FSignal
instance SArith FSample Scalar FSample
instance SArith FDistrib Scalar FDistrib
instance SArith FClass Scalar FClass

instance SArith Signal Signal Signal
instance SArith Sample Sample Sample
instance SArith FSignal FSignal FSignal
instance SArith FSample FSample FSample
instance SArith FDistrib FDistrib FDistrib
instance SArith FClass FClass FClass

instance SArith TestRow TestRow TestRow
instance SArith Scalar TestRow TestRow
instance SArith TestRow Scalar TestRow

class (SArith s1 s2 s3, DZipWith c1 c2 c3 d1 d2 d3) => SZipWith s1 s2 s3 c1 c2 c3 d1 d2 d3 | s1 s2 -> s3, c1 c2 -> c3  where
       szipWith ::  (d1 -> d2 -> d3) -> TC s1 typ1 (c1 d1) -> TC s2 typ2 (c2 d2) -> TC s3 typ3 (c3 d3)

instance (SArith s1 s2 s3, DZipWith c1 c2 c3 d1 d2 d3) => SZipWith s1 s2 s3 c1 c2 c3 d1 d2 d3 where
       szipWith f (TC da1) (TC da2) = TC $ dzipWith f da1 da2       

----------------------------------------------------------------
-- Getyptes ZipWith
stzipWith ::SZipWith s1 s2 s3 c1 c2 c3 d1 d2 d3 => (TC Scalar typ1 (Data Nil d1) -> TC Scalar typ2 (Data Nil d2) -> TC Scalar typ3 (Data Nil d3)) -> TC s1 typ1 (c1 d1) -> TC s2 typ2 (c2 d2) -> TC s3 typ3 (c3 d3)        
stzipWith f xs ys = szipWith g xs ys where g x y = fromScalar $ f (toScalar x) (toScalar y) 


----------------------------------------------------------------
-- Signal crosswith with Rule of Signal Inheritance
class SCrossArith s1 s2 s3 | s1 s2 -> s3
instance SCrossArith Signal Sample Signal
instance SCrossArith Sample Signal Sample
instance SCrossArith FSignal FSample FSignal
instance SCrossArith FSample FSignal FSample
instance SCrossArith FDistrib FClass FDistrib 
instance SCrossArith FClass FDistrib FClass

class (SCrossArith s1 s2 s3, DCrossWith c1 c2 c3 d1 d2 d3) => SCrossWith s1 s2 s3 c1 c2 c3 d1 d2 d3 | s1 s2 -> s3, c1 c2 -> c3  where
       scrossWith ::  (d1 -> d2 -> d3) -> TC s1 typ1 (c1 d1) -> TC s2 typ2 (c2 d2) -> TC s3 typ3 (c3 d3)

instance (SCrossArith s1 s2 s3, DCrossWith c1 c2 c3 d1 d2 d3) => SCrossWith s1 s2 s3 c1 c2 c3 d1 d2 d3 where
       scrossWith f (TC da1) (TC da2) = TC $ dcrossWith f da1 da2       

----------------------------------------------------------
-- Normal Arithmetics - based on zip

class ZipProd s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (.*) :: TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (./) :: TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3
     
instance  (DZipWith v1 v2 v3 d1 d2 d3, BProd d1 d2 d3, SArith s1 s2 s2, TProd t1 t2 t3) =>  ZipProd s1 s2 s2 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (.*) x y = szipWith (..*) x y
          (./) x y = szipWith (../) x y

class ZipSum s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (.+) ::  TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (.-) ::  TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3

instance  (DZipWith v1 v2 v3 d1 d2 d3, BSum d1 d2 d3, SArith s1 s2 s3, TSum t1 t2 t3) =>  ZipSum s1 s2 s3 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (.+) x y = szipWith (..+) x y
          (.-) x y = szipWith (..-) x y

infix 7 .*, ./
infix 6 .+,.-


----------------------------------------------------------
-- Cross Arithmetics - based on crossWith

class CrossProd s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (&*) :: TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (&/) :: TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3
     
instance  (SCrossWith s1 s2 s3 v1 v2 v3 d1 d2 d3, BProd d1 d2 d3, TProd t1 t2 t3) =>  CrossProd s1 s2 s3 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (&*) x y = scrossWith (..*) x y
          (&/) x y = scrossWith (../) x y

class CrossSum s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (&+) :: TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (&-) :: TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3
     
instance  (SCrossWith s1 s2 s3 v1 v2 v3 d1 d2 d3, BSum d1 d2 d3, TProd t1 t2 t3) =>  CrossSum s1 s2 s3 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (&+) x y = scrossWith (..+) x y
          (&-) x y = scrossWith (..-) x y
          

infix 7 &*, &/
infix 6 &+, &-

----------------------------------------------------------
-- Convenience Type Synonyms

-- generic
type Scal typ a = TC Scalar typ (DVal a)
          
type Sig1 typ a = TC Signal typ (UVec a)
type FSig1 typ a = TC FSignal typ (UVec a)
type Sig1L typ a = TC Signal typ (List a)
type Sig2L typ a = TC Signal typ (List2 a)

type Sig2 typ a = TC Signal typ (Vec2 a)
type FSig2 typ a = TC FSignal typ (Vec2 a)

type Test1 typ a = TC TestRow typ (UVec a)
type Test2 typ a = TC TestRow typ (UVec2 a)

-- specific
--type UTignal a = Sig1 (Typ UT UT UT) a
--type FUTSignal = FSig1 (Typ UT UT UT) Val

-- type UTSignal a = Sig1 (Typ UT UT UT) a
type PSig = Sig1 (Typ A P Tt) Val
type PSig2 = Sig2 (Typ A P Tt) Val
type PSigL = Sig1L (Typ A P Tt) Val
type PSig2L = Sig2L (Typ A P Tt) Val

type TSig = Sig1 (Typ A T Tt) Val
type TSigL = Sig1L (Typ A T Tt) Val
type FSig = FSig1 (Typ A F Tt) Val
type DTSig = FSig1 (Typ D T Tt) Val
type UTSig = Sig1 (Typ UT UT UT) Val

type UTFSig = FSig1 (Typ UT UT UT) Val
type UTSigL = Sig1L (Typ UT UT UT) Val
  
type PVal = Scal (Typ A P Tt) Val
type TVal = Scal (Typ A T Tt) Val
type FVal = Scal (Typ A F Tt) Val
type DTVal = Scal (Typ D T Tt) Val 

type SignalIdx = Int

-- type TSample = TC Scalar (Typ A T Tt) (DVal Val)
type PSample2 = TC Sample (Typ A P Tt) (UVec2 Val)
type PSample1 =  TC Sample (Typ A P Tt) (UVec Val)
type PSample1L =  TC Sample (Typ A P Tt) (List Val)
type PSample2L = TC Sample (Typ A P Tt) (UVec2L Val)
type PSample2LL = TC Sample (Typ A P Tt) (List2 Val)
type PSample = TC Sample (Typ A P Tt) (DVal Val)


type DTSampleL = TC Sample (Typ D T Tt) (List Val) 


-- type PSample =  TC Scalar (Typ A P Tt) (DVal Val)
type DTSample =  TC Sample (Typ D T Tt) (DVal Val)
type TSample =  TC Sample (Typ A T Tt) (DVal Val)
type TSample1 =  TC Sample (Typ A T Tt) (UVec Val)
type TSample1L =  TC Sample (Typ A T Tt) (List Val)


------------------------------------

type RSig = (TSigL, PSample2LL)
type RSample1 = (TSample, PSample1L)
type RSample = (TSample, PSample)


rhead :: RSig -> RSample1  
rhead (t,ps) = (shead t, shead ps) 

rtail :: RSig -> RSig
rtail (t,ps) = (stail t, stail ps) 
  
rlast :: RSig -> RSample1
rlast (t,ps) = (slast t, slast ps) 

rinit :: RSig -> RSig
rinit (t,ps) = (sinit t, sinit ps) 

rsingleton :: RSample1 -> RSig
rsingleton (t,ps) = (ssingleton t, ssingleton ps)
  
  
  
-- xappend :: RSig -> RSig -> RSig
-- xappend (t1,ps1) (t2,ps2) = (t1.++t2, ps1.++ps2)

-- xconcat xs = L.foldl' (xappend) []

-- instance Monoid RSig where
--   mempty = (mempty,mempty) 
--   mappend (t1,ps1) (t2,ps2) = (t1 .++ t2, ps1 .++ ps2) 

----------------------------------------------------------
-- from/to List

sunpack :: TC s t (c d) -> (c d) 
sunpack (TC x) = x

sfromList :: DFromList c d => [d] -> TC s t (c d)
sfromList x = TC $ dfromList x

sfromVal :: DFromList c d => Int -> d -> TC s t (c d)
sfromVal len x = sfromList (replicate len x) 

stoList :: DFromList c d => TC t t1 (c d) -> [d]
stoList (TC x) = dtoList x

fromScalar :: TC Scalar typ (Data Nil d) -> d 
fromScalar (TC (Data (D0 x))) = x

toScalar :: d -> TC Scalar typ (Data Nil d) 
toScalar x = TC $ Data $ D0 x

toSample :: d -> TC Sample typ (Data Nil d)
toSample x = TC $ Data $ D0 x

fromSample :: TC Sample typ (Data Nil d) -> d
fromSample (TC (Data (D0 x))) = x


class SConst s c d where 
      toConst :: Int -> d -> TC s (Typ UT UT UT) (c d) 
  
instance (VFromList v1 Double) => SConst Signal (Data (v1 :> Nil)) Val where   
         toConst len x =  sfromVal len x
  
instance (VFromList v1 Double) => SConst FSignal (Data (v1 :> Nil)) Val where   
         toConst len x =  sfromVal len x

instance SConst Scalar (Data Nil) Val where   
         toConst len x =  toScalar x

getSigVec :: TC Signal typ (Data (v1 :> Nil) d) -> v1 d
getSigVec (TC (Data (D1 x))) = x

vec2Sig :: v1 d -> TC Signal typ (Data (v1 :> Nil) d)
vec2Sig x = (TC (Data (D1 x)))

{-
fromSigList :: [TC Signal typ (Data (v1 :> Nil) d)] -> TC Signal typ (Data (V.Vector :> v1 :> Nil) d)
fromSigList slist = TC $ Data $ D2 $ V.fromList vecList
            where vecList = map getSigVec slist

toSigList :: TC Signal typ (Data (V.Vector :> v1 :> Nil) d) -> [TC Signal typ (Data (v1 :> Nil) d)]
toSigList (TC (Data (D2 x))) = map vec2Sig vecList
            where vecList = V.toList x
-}
----------------------------------------------------------
-- Zip
                                                                        
szip :: (SArith s s s, DZipWith c c c d1 d2 (d1, d2)) => TC s typ (c d1) ->  TC s typ (c d2) ->  TC s typ (c (d1,d2))
szip x y = szipWith ((,)) x y                

----------------------------------------------------------
-- SMap
class DMap c d1 d2 => SMap c d1 d2 where
      smap :: (d1 -> d2) -> TC s typ (c d1) -> TC s typ (c d2)

instance DMap c d1 d2 => SMap c d1 d2 where
         smap f (TC x) = TC $ dmap f x 

----------------------------------------------------------
-- Getyptes SMap
class SMap c d1 d2 => STMap c d1 d2 typ1 typ2 | typ1 -> typ2, typ2 -> typ1 where
      stmap :: (TC Sample typ1 (Data Nil d1) -> TC Sample typ2 (Data Nil d2)) -> TC  s typ1 (c d1) -> TC s typ2 (c d2)

instance SMap c d1 d2 => STMap c d1 d2 typ1 typ2 where
         stmap f xs = changeType $ smap (fromSample . f . toSample) xs

----------------------------------------------------------
-- DeltaMap

class SDeltaMap s1 s2 c d1 d2 where
      sdeltaMap :: (d1 -> d1 -> d2) -> TC s1 typ (c d1) -> TC s2 typ (c d2)
      sdeltaMapReverse ::  (d1 -> d1 -> d2) -> TC s1 typ (c d1) -> TC s2 typ (c d2)

instance (DZipWith c c c d1 d1 d2, STail Signal c d1) => SDeltaMap Signal FSignal c d1 d2 where
      sdeltaMap f x = changeSignalType $ szipWith f  x (stail x) 
      sdeltaMapReverse f x = changeSignalType $ szipWith f (stail x) x 

----------------------------------------------------------
-- Getyptes DeltaMap

class STDeltaMap s1 s2 c d1 d2 where
      stdeltaMap :: (TC Scalar typ1 (Data Nil d1) -> TC Scalar typ1 (Data Nil d1) -> TC Scalar typ2 (Data Nil d2)) -> TC s1 typ1 (c d1) -> TC s2 typ2 (c d2)
      stdeltaMapReverse :: (TC Scalar typ1 (Data Nil d1) -> TC Scalar typ1 (Data Nil d1) -> TC Scalar typ2 (Data Nil d2)) -> TC s1 typ1 (c d1) -> TC s2 typ2 (c d2)

instance (SDeltaMap s1 s2 c d1 d2) => STDeltaMap s1 s2 c d1 d2 where
      stdeltaMap f xs = changeType $ sdeltaMap g xs where g x y = fromScalar $ f (toScalar x) (toScalar y)
      stdeltaMapReverse f xs = changeType $ sdeltaMapReverse g xs where g x y = fromScalar $ f (toScalar x) (toScalar y)


----------------------------------------------------------
-- Doppeltes Getyptes DeltaMap -- deltaMap ueber powerRecord
{-
class  StDeltaMap2 s1 s2 s3 c1 c2 c3 d1 d2 d3 where
      stdeltaMap2 :: (TC Scalar typ1 (Data Nil (d1,d1))-> TC Scalar typ2 (Data Nil (d2,d2)) -> TC Scalar typ3 (Data Nil d3))  ->  TC s1 typ1 (c1 d1) ->  TC s2 typ2 (c2 d2) ->  TC s3 typ3 (c3 d3)
      stdeltaMap2Reverse :: (TC Scalar typ1 (Data Nil (d1,d1))-> TC Scalar typ2 (Data Nil (d2,d2)) -> TC Scalar typ3 (Data Nil d3))  ->  TC s1 typ1 (c1 d1) ->  TC s2 typ2 (c2 d2) ->  TC s3 typ3 (c3 d3)
      
instance (SArith s1 s1 s1, DZipWith c1 c1 c1 (d1, d1) (d2, d2) d3, DZipWith c1 c1 c1 d1 d1 (d1, d1), DZipWith c1 c1 c1 d2 d2 (d2, d2), STail s1 c1 d1, STail s1 c1 d2) =>  StDeltaMap2 s1 s1 s1 c1 c1 c1 d1 d2 d3 where
  stdeltaMap2 f xs ys = changeSignalType $ (stzipWith f dxs dys)   
    where dxs = szipWith ((,)) xs (stail xs)
          dys = szipWith ((,)) ys (stail ys)
  stdeltaMap2Reverse f xs ys = changeSignalType $ (stzipWith f dxs dys)   
    where dxs = szipWith ((,)) (stail xs) xs
          dys = szipWith ((,)) (stail ys) ys


class  StDeltaMap2 s1 s2 s3 c1 c2 c3 d1 d2 d3 where
      stdeltaMap2 :: (TC Scalar typ1 (Data Nil (d1,d1))-> TC Scalar typ2 (Data Nil (d2,d2)) -> TC Scalar typ3 (Data Nil d3))  ->  TC s1 typ1 (c1 d1) ->  TC s2 typ2 (c2 d2) ->  (TC s3 typ3 (c3 d3),
      stdeltaMap2Reverse :: (TC Scalar typ1 (Data Nil (d1,d1))-> TC Scalar typ2 (Data Nil (d2,d2)) -> TC Scalar typ3 (Data Nil d3))  ->  TC s1 typ1 (c1 d1) ->  TC s2 typ2 (c2 d2) ->  TC s3 typ3 (c3 d3)
      
instance (SArith s1 s1 s1, DZipWith c1 c1 c1 (d1, d1) (d2, d2) d3, DZipWith c1 c1 c1 d1 d1 (d1, d1), DZipWith c1 c1 c1 d2 d2 (d2, d2), STail s1 c1 d1, STail s1 c1 d2) =>  StDeltaMap2 s1 s1 s1 c1 c1 c1 d1 d2 d3 where
  stdeltaMap2 f xs ys = changeSignalType $ (stzipWith f dxs dys)   
    where dxs = szipWith ((,)) xs (stail xs)
          dys = szipWith ((,)) ys (stail ys)
  stdeltaMap2Reverse f xs ys = changeSignalType $ (stzipWith f dxs dys)   
    where dxs = szipWith ((,)) (stail xs) xs
          dys = szipWith ((,)) (stail ys) ys
-}
---------------------------------------------------------
-- sFold

class (DFold c d1 d2) => SFold s c d1 d2 where
  sfoldl :: (d1 -> d2 -> d1) ->  d1 -> TC s typ (c d2) -> d1
  sfoldr :: (d1 -> d2 -> d2) ->  d2 -> TC s typ (c d1) -> d2
  sfoldl f x (TC y) = dfoldl f x y 
  sfoldr f x (TC y) = dfoldr f x y 

instance (DFold c d1 d2) => SFold Signal c d1 d2 where
instance (DFold c d1 d2) => SFold FSignal c d1 d2 where
instance (DFold c d1 d2) => SFold FSample c d1 d2 where
instance (DFold c d1 d2) => SFold FDistrib c d1 d2 where
instance (DFold c d1 d2) => SFold FClass c d1 d2 where

{-
  ----------------------------------------------------------
-- sTFold
class STFold s1 s2 c1 c2 d1 d2 where
  stfoldl :: (TC s1 typ1 (c1 d1) -> TC s2 typ2 (c2 d2) -> TC s1 typ1 (c1 d1)) ->  TC s1 typ1 (c1 d1) -> TC s2 typ2 (c2 d2) -> TC s1 typ1 (c1 d1)

instance (SFold Scalar (Data Nil) d1 d2) => STFold Scalar s2 (Data Nil) c2 d1 d2 where
   stfoldl f a x = toScalar $ sfoldl g (fromScalar a) x where g a x = fromScalar $ f (toScalar a) (sunpack x)
-}

----------------------------------------------------------
-- SHead & STail

class (DHead c1 c2 d) => SHead s1 s2 c1 c2 d | s1 -> s2 where
      shead ::  TC s1 typ (c1 d) -> TC s2 typ (c2 d)
      slast ::  TC s1 typ (c1 d) -> TC s2 typ (c2 d)
      shead (TC x) = TC $ dhead x   
      slast (TC x) = TC $ dlast x   

instance (DHead c1 c2 d) => SHead Signal Sample c1 c2 d where
instance (DHead c1 c2 d) => SHead FSignal FSample c1 c2 d where
instance (DHead c1 c2 d) => SHead Sample Sample c1 c2 d where


class (DTail c c d) => STail s c d where
      sinit ::  TC s typ (c d) -> TC s typ (c d)
      stail ::  TC s typ (c d) -> TC s typ (c d)
      sinit (TC x) = TC $ dinit x   
      stail (TC x) = TC $ dtail x   

instance (DTail c c d) => STail Signal c d where
instance (DTail c c d) => STail FSignal c d where
instance (DTail c c d) => STail Sample c d where


----------------------------------------------------------
-- SHead & STail

class (DTranspose c d) => STranspose s1 s2 c d | s1 -> s2, s2 -> s1 where
      stranspose ::  TC s1 typ (c d) -> TC s2 typ (c d)
      stranspose (TC x) = TC $ dtranspose x
      
instance (DTranspose c d) => STranspose Signal Sample  c d
instance (DTranspose c d) => STranspose Sample Signal  c d
instance (DTranspose c d) => STranspose FSignal FSample  c d
instance (DTranspose c d) => STranspose FSample FSignal  c d

----------------------------------------------------------
-- Monoid

instance Monoid c => Monoid (TC s typ c) where
  mempty = TC $ mempty
  mappend (TC x) (TC y) = TC $ mappend x y 
 

class DAppend c1 c2 c3 d => SAppend s1 s2 s3 c1 c2 c3 d | s1 s2 -> s3 where
  sappend :: TC s1 t (c1 d) -> TC s2 t (c2 d) -> TC s3 t (c3 d)
  sappend (TC x) (TC y) = TC $ dappend x y   

  
class SSingleton s1 s2 c1 c2 d | s2 -> s1, c1 -> c2, c2 -> c1  where 
  ssingleton :: TC s1 t (c1 d) -> TC s2 t (c2 d)
  
instance (VSingleton v1 d) => SSingleton Sample Signal (Data (Nil)) (Data (v1:> Nil)) d where  
  ssingleton (TC x) = TC $ dsingleton x 

instance (VSingleton v2 (v1 d)) => SSingleton Sample Signal (Data (v1 :> Nil)) (Data (v2 :> v1:> Nil)) d where  
  ssingleton (TC x) = TC $ dsingleton x 

instance (VSingleton v2 (v1 d)) => SSingleton Sample Sample (Data (v1 :> Nil)) (Data (v2 :> v1:> Nil)) d where  
  ssingleton (TC x) = TC $ dsingleton x 

instance DAppend c1 c2 c3 d => SAppend Signal Signal Signal c1 c2 c3 d 
instance DAppend c1 c2 c3 d => SAppend Signal Sample Signal c1 c2 c3 d
instance DAppend c1 c2 c3 d => SAppend Sample Signal Signal c1 c2 c3 d


-- (.++) :: SAppend s1 s2 s3 c1 c2 c3 d =>  (TC s1 typ (c1 d)) -> (TC s2 typ (c2 d)) -> (TC s3 typ (c3 d))
-- (.++) x y = sappend x y

(.++) x y = mappend x y 

infix 5 .++

----------------------------------------------------------
-- All & Co

class SAll s c d where
  sall :: (d -> Bool) -> TC s typ (c d) -> Bool
  sany :: (d -> Bool) -> TC s typ (c d) -> Bool

instance DAll c d => SAll s c d where
  sall f (TC x) = dall f x 
  sany f (TC x) = dany f x 
    
----------------------------------------------------------
-- signal sign

--sigSign :: (Ord d1, Num d1, SBox s c c d1, SMap s c d1 Sign) => TC s typ (c d1) -> TC s typ (c Sign)
sigSign x = smap sign $ sbox x

----------------------------------------------------------
-- box / unbox a signal

class SBox s1 c1 c2 d1 where
   sbox :: TC s1 typ (c1 d1) -> TC s1 typ (c2 d1)
   sunbox :: TC s1 typ (c2 d1) -> TC s1 typ (c1 d1)

instance (UV.Unbox d1) => SBox Signal (Data (UV.Vector :> Nil)) (Data (V.Vector :> Nil)) d1  where
         sbox (TC(Data(D1 x))) = TC $ Data $ D1 $ vbox x
         sunbox (TC(Data(D1 x))) = TC $ Data $ D1 $ vunbox x

instance (UV.Unbox d1) => SBox Scalar c1 c1 d1 where
         sbox x = x
         sunbox x = x

----------------------------------------------------------
-- sum all signal value

class (BSum d1 d1 d1, Num d1) => SigSum s1 s2 c1 c2 d1 | s1 -> s2, c1 -> c2  where
      sigSum :: TC s1 typ (c1 d1) -> TC s2 typ (c2 d1)
    
instance  (BSum d1 d1 d1, Num d1, VWalker v1 d1 d1) => SigSum Signal Scalar (Data (v1 :> Nil)) (Data Nil) d1 where
         sigSum x = TC $ Data $ D0 $ sfoldl (..+) 0 x 

instance  (BSum d1 d1 d1, Num d1, VWalker v1 d1 d1) =>  SigSum FSignal Scalar (Data (v1 :> Nil)) (Data Nil) d1 where
         sigSum x = TC $ Data $ D0 $ sfoldl (..+) 0 x 

----------------------------------------------------------
-- Delta and 2Point Average of Signal

class (DSucc delta1 delta2) => DeltaSig s1 s2 c1 delta1 delta2 d1  | s1 -> s2 where
      deltaSig :: TC s1 (Typ delta1 t1 p1) (c1 d1) -> TC s2 (Typ delta2 t1 p1) (c1 d1)
      avSig ::  TC s1 (Typ delta1 t1 p1) (c1 d1) -> TC s2 (Typ delta1 t1 p1) (c1 d1)

instance (SDeltaMap Signal FSignal c1 Val Val, DSucc delta1 delta2) => DeltaSig Signal FSignal c1 delta1 delta2 Val where
      deltaSig x = changeDelta $ sdeltaMapReverse (..-) x
      avSig x = changeDelta $ sdeltaMapReverse (\ x1 x2 -> (x1..+x2)../ (2::Val)) x


ssort ::  (DSort c1 d1) =>  TC s1 typ (c1 d1) ->  TC s1 typ (c1 d1)
ssort (TC x) = TC $ dsort x 


----------------------------------------------------------
-- Part & Full Integrate 

-- DeltaSig Signal FSignal (Data (v1 :> Nil)) A D Val => 
-- | Partial Signal Integration        
sigPartInt ::  TSig -> PSig -> FSig
sigPartInt time power = (deltaSig time) .* (avSig power)
-- czipWith (*) dTime $ dmap (\ p1 p2 -> (p1+p2)/2) power 


-- | Partial Signal Integration
sigFullInt ::  TSig -> PSig -> FSig
sigFullInt time power = sfromList [fromScalar $ sigSum $ sigPartInt time power]

-- csingleton (cfoldr (+) 0  $ czipWith (*) dTime $ dmap (\ p1 p2 -> (p1+p2)/2) power)

----------------------------------------------------------
-- | make untyped
untype ::  TC s1 (Typ delta1 t1 p1) (c1 d1) -> TC s1 (Typ UT UT UT) (c1 d1)
untype (TC x) = TC x

-- | make typed
setType ::  TC s1 (Typ UT UT UT) (c1 d1) -> TC s1 (Typ delta1 t1 p1) (c1 d1)
setType (TC x) = TC x

-- | 
setTypeTestRow ::  TC sig ty val -> TC TestRow ty val
setTypeTestRow (TC x) = TC x

-- | change the Type
changeType :: TC s typ1 (c d) -> TC s typ2 (c d)
changeType (TC x) = TC x

-- | change the Type
changeDelta :: TC s (Typ delta1 t p) (c d) -> TC s (Typ delta2 t p) (c d)
changeDelta (TC x) = TC x

-- | change the signal type
changeSignalType :: TC s1 typ (c d) ->  TC s2 typ (c d)
changeSignalType (TC x) = TC x

-- | sneg :: (DArith0 d, SMap s c d d) => TC s typ (c d) -> TC s typ (c d)
sneg :: (DArith0 d, SMap c d d) => TC s typ (c d) -> TC s typ (c d)
sneg = smap neg

-- | srec :: (DArith0 d, SMap s c d d) => TC s typ (c d) -> TC s typ (c d)
srec :: (DArith0 d, SMap c d d) => TC s typ (c d) -> TC s typ (c d)
srec = smap rec


-- | data ConversiSon function
fromSigList ::VFromList v2 (v1 d)=> [TC s typ (Data (v1 :> Nil) d)] -> TC s typ (Data (v2 :> v1 :> Nil) d) 
fromSigList xs = TC $ Data $ D2 $ vfromList $ map f xs
  where f (TC (Data (D1 x))) = x
        
-- | data Conversion function
toSigList :: (VFromList v2 (TC s typ (Data (v1 :> (Nil' :> Nil')) d)), VWalker v2 (v1 d) (TC s typ (Data (v1 :> (Nil' :> Nil')) d))) => TC s typ (Data (v2 :> v1 :> Nil) d) -> [TC s typ (Data (v1 :> Nil) d)]
toSigList (TC (Data (D2 xs))) = vtoList $ vmap f xs
  where f x = TC $ Data $ D1 x
       

sfilter ::  (VFilter v1 d) => (d -> Bool) -> TC s typ (Data (v1 :> Nil) d) -> TC s typ (Data (v1 :> Nil) d)
sfilter f (TC x) = TC $ dfilter f x


sampleAverage :: Fractional d => TC Sample typ (Data Nil d) -> TC Sample typ (Data Nil d) -> TC Sample typ (Data Nil d)
sampleAverage (TC (Data (D0 x))) (TC (Data (D0 y))) = TC $ Data $ D0 $ (x+y)/2


  
  