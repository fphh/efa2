{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, TypeOperators,  GADTs, OverlappingInstances, FlexibleContexts #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where
import EFA2.Signal.Vector
import EFA2.Signal.Base
import EFA2.Signal.Data
import EFA2.Signal.Typ
-- import EFA2.Signal.Vector
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Monoid

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

----------------------------------------------------------------
-- Signal Zipwith with Rule of Signal Inheritance
class SArith s1 s2 s3 | s1 s2 -> s3

instance SArith Scalar Scalar Scalar

instance SArith Scalar Signal Signal
instance SArith Scalar FSignal FSignal
instance SArith Scalar FSample FSample
instance SArith Scalar FDistrib FDistrib
instance SArith Scalar FClass FClass

instance SArith Signal Scalar Signal
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


class (SArith s1 s2 s3, DZipWith c1 c2 c3 d1 d2 d3) => SZipWith s1 s2 s3 c1 c2 c3 d1 d2 d3 | s1 s2 -> s3, c1 c2 -> c3  where
       szipWith ::  (d1 -> d2 -> d3) -> TC s1 typ1 (c1 d1) -> TC s2 typ2 (c2 d2) -> TC s3 typ3 (c3 d3)

instance (SArith s1 s2 s3, DZipWith c1 c2 c3 d1 d2 d3) => SZipWith s1 s2 s3 c1 c2 c3 d1 d2 d3 where
       szipWith f (TC da1) (TC da2) = TC $ dzipWith f da1 da2       

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
     
instance  (DZipWith v1 v2 v3 d1 d2 d3, DArith d1 d2 d3, SArith s1 s2 s2, TProd t1 t2 t3) =>  ZipProd s1 s2 s2 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (.*) x y = szipWith (..*) x y
          (./) x y = szipWith (../) x y

class ZipSum s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (.+) ::  TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (.-) ::  TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3

instance  (DZipWith v1 v2 v3 d1 d2 d3, DArith d1 d2 d3, SArith s1 s2 s3, TSum t1 t2 t3) =>  ZipSum s1 s2 s3 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (.+) x y = szipWith (..+) x y
          (.-) x y = szipWith (..-) x y

{-
class Prod s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (.*) :: TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (./) :: TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3
     
instance  (DZipWith v1 v2 v3 d1 d2 d3, DArith d1 d2 d3, SArith s1 s2 s2, TProd t1 t2 t3) =>  Prod s1 s2 s2 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (.*) (TC x) (TC y) = TC $ dzipWith (..*) x y
          (./) (TC x) (TC y) = TC $ dzipWith (..*) x y

class Sum s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (.+) ::  TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (.-) ::  TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3

instance  (DZipWith v1 v2 v3 d1 d2 d3, DArith d1 d2 d3, SArith s1 s2 s3, TSum t1 t2 t3) =>  Sum s1 s2 s3 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (.+) (TC x) (TC y) = TC $ dzipWith (..+) x y
          (.-) (TC x) (TC y) = TC $ dzipWith (..-) x y
-}
----------------------------------------------------------
-- Cross Arithmetics - based on crossWith

class CrossProd s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (&*) :: TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (&/) :: TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3
     
instance  (SCrossWith s1 s2 s3 v1 v2 v3 d1 d2 d3, DArith d1 d2 d3, TProd t1 t2 t3) =>  CrossProd s1 s2 s3 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (&*) x y = scrossWith (..*) x y
          (&/) x y = scrossWith (../) x y

class CrossSum s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (&+) :: TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (&-) :: TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3
     
instance  (SCrossWith s1 s2 s3 v1 v2 v3 d1 d2 d3, DArith d1 d2 d3, TProd t1 t2 t3) =>  CrossSum s1 s2 s3 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (&+) x y = scrossWith (..*) x y
          (&-) x y = scrossWith (../) x y
          
----------------------------------------------------------
-- Convenience Type Synonyms

-- generic
type Scal typ a = TC Scalar typ (DVal a)
          
type Sig1 typ a = TC Signal typ (UVec a)
type FSig1 typ a = TC FSignal typ (UVec a)

type Sig2 typ a = TC Signal typ (Vec a)
type FSig2 typ a = TC FSignal typ (Vec2 a)


-- specific
--type UTSignal a = Sig1 (Typ UT UT UT) a
--type FUTSignal = FSig1 (Typ UT UT UT) Val

-- type UTSignal a = Sig1 (Typ UT UT UT) a
type PSig = Sig1 (Typ A P Tt) Val
type TSig = Sig1 (Typ A T Tt) Val
type FSig = FSig1 (Typ A F Tt) Val
type DTSig = FSig1 (Typ D T Tt) Val
type UTSig a = Sig1 (Typ UT UT UT) a
type UTFSig = FSig1 (Typ UT UT UT) Val

type PVal = Scal (Typ A P Tt) Val
type TVal = Scal (Typ A T Tt) Val
type FVal = Scal (Typ A F Tt) Val
type DTVal = Scal (Typ D T Tt) Val 

type SignalIdx = Int

type PSample2 = TC Sample (Typ A P Tt) (UVec2 Val)
type PSample1 =  TC Scalar (Typ A P Tt) (DVal Val)
type PSample = Val

-- type TSample0 =  TC Scalar (Typ A P Tt) (DVal Val))
-- type PSample0 =  TC Scalar (Typ A P Tt) (DVal Val))


----------------------------------------------------------
-- from/to List

sfromList :: FromToList c d => [d] -> TC s t (c d)
sfromList x = TC $ dfromList x

sfromVal :: FromToList c d => Int -> d -> TC s t (c d)
sfromVal len x = sfromList (replicate len x) 

stoList :: FromToList c d => TC t t1 (c d) -> [d]
stoList (TC x) = dtoList x

fromScalar :: TC Scalar typ (Data Nil d) -> d 
fromScalar (TC (Data (D0 x))) = x

toScalar :: d -> TC Scalar typ (Data Nil d) 
toScalar x = TC $ Data $ D0 x

getSigVec :: TC Signal typ (Data (v1 :> Nil) d) -> v1 d
getSigVec (TC (Data (D1 x))) = x

vec2Sig :: v1 d -> TC Signal typ (Data (v1 :> Nil) d)
vec2Sig x = (TC (Data (D1 x)))

fromSigList :: [TC Signal typ (Data (v1 :> Nil) d)] -> TC Signal typ (Data (V.Vector :> v1 :> Nil) d)
fromSigList slist = TC $ Data $ D2 $ V.fromList vecList
            where vecList = map getSigVec slist

toSigList :: TC Signal typ (Data (V.Vector :> v1 :> Nil) d) -> [TC Signal typ (Data (v1 :> Nil) d)]
toSigList (TC (Data (D2 x))) = map vec2Sig vecList
            where vecList = V.toList x

----------------------------------------------------------
-- SMap
class DMap c d1 d2 => SMap c d1 d2 where
      smap :: (d1 -> d2) -> TC s typ (c d1) -> TC s typ (c d2)

instance DMap c d1 d2 => SMap c d1 d2 where
         smap f (TC x) = TC $ dmap f x 

----------------------------------------------------------
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

----------------------------------------------------------
-- DeltaMap

class SDeltaMap s1 s2 c1 d1 d2 | s1 -> s2 where
      sdeltaMap :: (d1 -> d1 -> d2) -> TC s1 typ1 (c1 d1) -> TC s2 typ2 (c1 d2)
      sdeltaMap' ::  (d1 -> d1 -> d2) -> TC s1 typ1 (c1 d1) -> TC s2 typ2 (c1 d2)

instance (VWalker v1 d1 d2, VSingleton v1 d1, VZipper v1 d1 d1 d2) => SDeltaMap Signal FSignal (Data (v1 :> Nil)) d1 d2 where
      sdeltaMap f (TC (Data (D1 x))) = TC $ Data $ D1 $ vdeltaMap f x 
      sdeltaMap' f (TC (Data (D1 x))) = TC $ Data $ D1 $ vdeltaMapReverse f x 

----------------------------------------------------------
-- SHead & STail

class SHead s1 s2 c1 c2 d1 where
      shead ::  TC s1 typ (c1 d1) -> TC s2 typ (c2 d1)
      slast ::  TC s1 typ (c1 d1) -> TC s2 typ (c2 d1)

instance (VSingleton v1 d1) => SHead Signal Scalar (Data (v1 :> Nil)) (Data Nil) d1 where
         shead (TC (Data (D1 x))) = TC $ Data $ D0 $ vhead x   
         slast (TC (Data (D1 x))) = TC $ Data $ D0 $ vlast x   

class SInit s1 c1 d1 where
      sinit ::  TC s1 typ (c1 d1) -> TC s1 typ (c1 d1)
      stail ::  TC s1 typ (c1 d1) -> TC s1 typ (c1 d1)

instance (VSingleton v1 d1) => SInit Signal (Data (v1 :> Nil)) d1 where
         sinit (TC (Data (D1 x))) = TC $ Data $ D1 $ vinit x   
         stail (TC (Data (D1 x))) = TC $ Data $ D1 $ vtail x   

----------------------------------------------------------
-- SHead & STail

class STranspose s1 s2 c1 c2 d1 | s1 -> s2, s2 -> s1  where
      stranspose ::  TC s1 typ (c1 d1) -> TC s2 typ (c2 d1)

instance STranspose FSignal FSample (Data (v1 :> Nil)) (Data (v1 :> Nil)) d1 where
         stranspose (TC (Data (D1 x))) = (TC (Data (D1 x))) 

instance VTranspose v1 v2 d1 => STranspose FSignal FSample (Data (v2 :> v1 :> Nil)) (Data (v2 :> v1 :> Nil)) d1 where
         stranspose (TC (Data (D2 x))) = TC $ Data $ D2 $ vtranspose x 

----------------------------------------------------------
-- Monoid
instance Monoid c => Monoid (TC s typ c) where
  mappend (TC x) (TC y) = TC $ mappend x y 

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

class (DArith d1 d1 d1, Num d1) => SigSum s1 s2 c1 c2 d1 | s1 -> s2, c1 -> c2  where
      sigSum :: TC s1 typ (c1 d1) -> TC s2 typ (c2 d1)
    
instance  (DArith d1 d1 d1, Num d1, VWalker v1 d1 d1) => SigSum Signal Scalar (Data (v1 :> Nil)) (Data Nil) d1 where
         sigSum x = TC $ Data $ D0 $ sfoldl (..+) 0 x 

instance  (DArith d1 d1 d1, Num d1, VWalker v1 d1 d1) =>  SigSum FSignal Scalar (Data (v1 :> Nil)) (Data Nil) d1 where
         sigSum x = TC $ Data $ D0 $ sfoldl (..+) 0 x 

----------------------------------------------------------
-- Delta and 2Point Average of Signal

class (DSucc delta1 delta2) => DeltaSig s1 s2 c1 delta1 delta2 d1  | s1 -> s2 where
      deltaSig :: TC s1 (Typ delta1 t1 p1) (c1 d1) -> TC s2 (Typ delta2 t1 p1) (c1 d1)
      avSig ::  TC s1 (Typ delta1 t1 p1) (c1 d1) -> TC s2 (Typ delta1 t1 p1) (c1 d1)

instance (SDeltaMap Signal FSignal c1 Val Val, DSucc delta1 delta2) => DeltaSig Signal FSignal c1 delta1 delta2 Val where
      deltaSig x = sdeltaMap' (..-) x
      avSig x = sdeltaMap' (\ x1 x2 -> (x1..+x2)../ (2::Val)) x

----------------------------------------------------------
-- Part & Full Integrate 

-- DeltaSig Signal FSignal (Data (v1 :> Nil)) A D Val => 
-- | Partial Signal Integration        
sigPartInt ::  TSig -> PSig -> FSig
sigPartInt time power = (deltaSig time) .* (avSig power)
-- czipWith (*) dTime $ dmap (\ p1 p2 -> (p1+p2)/2) power 


-- | Partial Signal Integration
sigFullInt ::  TSig -> PSig -> FVal
sigFullInt time power = sigSum $ sigPartInt time power -- csingleton (cfoldr (+) 0  $ czipWith (*) dTime $ dmap (\ p1 p2 -> (p1+p2)/2) power)

----------------------------------------------------------
-- make untyped


untype ::  TC s1 (Typ delta1 t1 p1) (c1 d1) -> TC s1 (Typ UT UT UT) (c1 d1)
untype (TC x) = TC x


--sneg :: (DArith0 d, SMap s c d d) => TC s typ (c d) -> TC s typ (c d)
sneg :: (DArith0 d, SMap c d d) => TC s typ (c d) -> TC s typ (c d)
sneg = smap neg

--srec :: (DArith0 d, SMap s c d d) => TC s typ (c d) -> TC s typ (c d)
srec :: (DArith0 d, SMap c d d) => TC s typ (c d) -> TC s typ (c d)
srec = smap rec
