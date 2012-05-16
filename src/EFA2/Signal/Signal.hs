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

-- type UTSignal a = Sig1 (Typ UT UT UT) a
type Power = Sig1 (Typ A P Tt) Val
type Time = Sig1 (Typ A T Tt) Val
type Flow = FSig1 (Typ A F Tt) Val
type DTime = FSig1 (Typ D T Tt) Val 
  

type SignalIdx = Int

----------------------------------------------------------
-- from/to List

sfromList x = TC $ dfromList x 
stoList (TC x) = dtoList x

fromScalar :: TC Scalar typ (Data Nil d) -> d 
fromScalar (TC (Data (D0 x))) = x

toScalar :: d -> TC Scalar typ (Data Nil d) 
toScalar x = TC $ Data $ D0 x


class SMap s c d1 d2 where
      smap :: (d1 -> d2) -> TC s typ (c d1) -> TC s typ (c d2)

instance (VWalker c d1 d2) => SMap s c d1 d2 where 
         smap f (TC x) = TC $ dmap f x


class SFold s1 s2 c1 c2 d1 d2 where
  sfoldl :: (d1 -> d2 -> d1) ->  TC s1 typ (c1 d1) -> TC s2 typ (c2 d2) -> TC s1 typ (c1 d1)  

instance (D0Fold c1 c2 d1 d2) => SFold Signal Scalar c1 c2 d1 d2 where
  sfoldl f (TC x) (TC y) = TC $ d0foldl f x y 

instance (D0Fold c1 c2 d1 d2) => SFold FSignal Scalar c1 c2 d1 d2 where
  sfoldl f (TC x) (TC y) = TC $ d0foldl f x y 

----------------------------------------------------------
-- signal sign

sigSign :: (VWalker c d1 Sign, Ord d1, Num d1) => TC s typ (c d1) -> TC s typ (c Sign)
sigSign x = smap sign x

----------------------------------------------------------
-- sum all signal value
{-
class SigSum s1 s2 where
  sigSum :: s1 -> s2

instance DSum c1 c2 d => SigSum (TC Signal typ (c1 d)) (TC Scalar typ (c2 d)) where
         sigSum (TC x) = TC (dsum x) 

instance DSum c1 c2 d => SigSum (TC FSignal typ (c1 d)) (TC Scalar typ (c2 d)) where
         sigSum (TC x) = TC (dsum x) 
-}

class (DArith d1 d1 d1, Num d1) => SigSum s1 s2 c1 c2 d1 where
      sigSum :: TC s1 typ (c1 d1) -> TC s2 typ (c2 d1)
    
instance (SFold Scalar Signal (Data Nil) (Data (v1 :> Nil)) d1 d1, DArith d1 d1 d1, Num d1) => SigSum Signal Scalar (Data (v1 :> Nil)) (Data Nil) d1 where
         sigSum x = sfoldl (..+) (toScalar 0) x 

instance  (SFold Scalar FSignal (Data Nil) (Data (v1 :> Nil)) d1 d1, DArith d1 d1 d1, Num d1) => SigSum FSignal Scalar (Data (v1 :> Nil)) (Data Nil) d1 where
         sigSum x = sfoldl (..+) (toScalar 0) x 
