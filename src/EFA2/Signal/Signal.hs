{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, TypeOperators,  GADTs, OverlappingInstances, FlexibleContexts #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where
import EFA2.Signal.Vector
import EFA2.Signal.Base
import EFA2.Signal.Data
import EFA2.Signal.Typ
-- import EFA2.Signal.Vector


----------------------------------------------------------
-- | Signal & Company

newtype TC s t d = TC d  deriving (Show, Eq, Ord)

data Scalar

data Signal
  
data FSignal
data FSample

data FDistrib
data FClass

-- Rule of Signal Inheritance
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
instance SArith FSignal FSignal FSignal
instance SArith FSample FSample FSample
instance SArith FDistrib FDistrib FDistrib
instance SArith FClass FClass FClass

  
----------------------------------------------------------
-- Signal Arithmetics

class Prod s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (.*) :: TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (./) :: TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3
     
instance  (DZipWith v1 v2 v3 d1 d2 d3, DArith d1 d2 d3, SArith s1 s2 s2, TProd t1 t2 t3) =>  Prod s1 s2 s2 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (.*) (TC x) (TC y) = TC $ dzipWith (..*) x y
          (./) (TC x) (TC y) = TC $ dzipWith (..*) x y

{-
instance  (DZipWith v1 v2 d1 d2 d3, DArith d1 d2 d3, SArith s1 s2 s1, TProd t1 t2 t3) =>  Prod s1 s2 s1 t1 t2 t3 (v1 d1) (v2 d2) (v2 d3) where
          (.*) (TC x) (TC y) = TC $ dzipWith (..*) x y
          (./) (TC x) (TC y) = TC $ dzipWith (..*) x y

instance  (DZipWith v1 v2 d1 d2 d3, DArith d1 d2 d3, SArith s1 s1 s1, TProd t1 t2 t3) =>  Prod s1 s1 s1 t1 t2 t3 (v1 d1) (v2 d2) (v2 d3) where
          (.*) (TC x) (TC y) = TC $ dzipWith (..*) x y
          (./) (TC x) (TC y) = TC $ dzipWith (..*) x y
-}

class Sum s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (.+) ::  TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (.-) ::  TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3

instance  (DZipWith v1 v2 v3 d1 d2 d3, DArith d1 d2 d3, SArith s1 s2 s3, TSum t1 t2 t3) =>  Sum s1 s2 s3 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (.+) (TC x) (TC y) = TC $ dzipWith (..+) x y
          (.-) (TC x) (TC y) = TC $ dzipWith (..-) x y


----------------------------------------------------------
-- Convenience Type Synonyms


-- generic
type Scal typ a = TC Scalar typ (DVal a)
          
type Sig1 typ a = TC Signal typ (UVec a)
type FSig1 typ a = TC FSignal typ (UVec a)

type Sig2 typ a = TC Signal typ (Vec a)
type FSig2 typ a = TC FSignal typ (Vec2 a)


-- specific
type UTSignal a = Sig1 (Typ UT UT UT) a
type FUTSignal = FSig1 (Typ UT UT UT) Val

-- type UTSignal a = Sig1 (Typ UT UT UT) a
type PSig = Sig1 (Typ A P Tt) Val
type TSig = Sig1 (Typ A T Tt) Val
type FSig = FSig1 (Typ A F Tt) Val
type DTSig = FSig1 (Typ D T Tt) Val 
  
type PVal = Scal (Typ A P Tt) Val
type TVal = Scal (Typ A T Tt) Val
type FVal = Scal (Typ A F Tt) Val
type DTVal = Scal (Typ D T Tt) Val 

type SignalIdx = Int

----------------------------------------------------------
-- from/to List

sfromList x = TC $ dfromList x 
stoList (TC x) = dtoList x

fromScalar :: TC Scalar typ (Data Nil d) -> d 
fromScalar (TC (Data (D0 x))) = x

toScalar :: d -> TC Scalar typ (Data Nil d) 
toScalar x = TC $ Data $ D0 x

----------------------------------------------------------
-- SMap

class SMap s c d1 d2 where
      smap :: (d1 -> d2) -> TC s typ (c d1) -> TC s typ (c d2)

instance (VWalker c d1 d2) => SMap s c d1 d2 where 
         smap f (TC x) = TC $ dmap f x


class SFold s1 s2 c1 c2 d1 d2 where
  sfoldl :: (d1 -> d2 -> d1) ->  TC s1 typ (c1 d1) -> TC s2 typ (c2 d2) -> TC s1 typ (c1 d1)  

instance (D0Fold c1 c2 d1 d2) => SFold Scalar Signal c1 c2 d1 d2 where
  sfoldl f (TC x) (TC y) = TC $ d0foldl f x y 

instance (D0Fold c1 c2 d1 d2) => SFold Scalar FSignal c1 c2 d1 d2 where
  sfoldl f (TC x) (TC y) = TC $ d0foldl f x y 

-- geht nicht hier fehlt die typ info !!!
-- ----------------------------------------------------------
-- -- sipWith 

-- class SipWith s1 s2 c1 c2 d1 d2 where
--       sipWith ::  TC s1 typ1 (c1 d1) -> TC s2 typ2 (c1 d2) -> TC s2 typ3 (c1 d2)

----------------------------------------------------------
-- DeltaMap

class SDeltaMap s1 s2 c1 d1 d2 | s1 -> s2 where
      sdeltaMap ::  (d1 -> d1 -> d2) -> TC s1 typ1 (c1 d1) -> TC s2 typ2 (c1 d2)
      sdeltaMap' ::  (d1 -> d1 -> d2) -> TC s1 typ1 (c1 d1) -> TC s2 typ2 (c1 d2)

instance (VWalker v1 d1 d2) => SDeltaMap Signal FSignal (Data (v1 :> Nil)) d1 d2 where
      sdeltaMap f (TC (Data (D1 x))) = TC $ Data $ D1 $ vdeltaMap f x 
      sdeltaMap' f (TC (Data (D1 x))) = TC $ Data $ D1 $ vdeltaMap' f x 

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
-- signal sign

sigSign :: (VWalker c d1 Sign, Ord d1, Num d1) => TC s typ (c d1) -> TC s typ (c Sign)
sigSign x = smap sign x

----------------------------------------------------------
-- sum all signal value

class (DArith d1 d1 d1, Num d1) => SigSum s1 s2 c1 c2 d1 | s1 -> s2, c1 -> c2  where
      sigSum :: TC s1 typ (c1 d1) -> TC s2 typ (c2 d1)
    
instance (SFold Scalar Signal (Data Nil) (Data (v1 :> Nil)) d1 d1, DArith d1 d1 d1, Num d1) => SigSum Signal Scalar (Data (v1 :> Nil)) (Data Nil) d1 where
         sigSum x = sfoldl (..+) (toScalar 0) x 

instance  (SFold Scalar FSignal (Data Nil) (Data (v1 :> Nil)) d1 d1, DArith d1 d1 d1, Num d1) => SigSum FSignal Scalar (Data (v1 :> Nil)) (Data Nil) d1 where
         sigSum x = sfoldl (..+) (toScalar 0) x 

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

