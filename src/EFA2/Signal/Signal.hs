{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, TypeOperators,  GADTs #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where
import EFA2.Signal.Vector
import EFA2.Signal.Base
import EFA2.Signal.Data
import EFA2.Signal.Typ


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
     
instance  (DZipWith v1 v2 d1 d2 d3, DArith d1 d2 d3, SArith s1 s2 s3, TProd t1 t2 t3) =>  Prod s1 s2 s3 t1 t2 t3 (v1 d1) (v2 d2) (v2 d3) where
          (.*) (TC x) (TC y) = TC $ dzipWith (..*) x y
          (./) (TC x) (TC y) = TC $ dzipWith (..*) x y

class Sum s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (.+) ::  TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (.-) ::  TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3

instance  (DZipWith v1 v2 d1 d2 d3, DArith d1 d2 d3, SArith s1 s2 s3, TSum t1 t2 t3) =>  Sum s1 s2 s3 t1 t2 t3 (v1 d1) (v2 d2) (v2 d3) where
          (.+) (TC x) (TC y) = TC $ dzipWith (..+) x y
          (.-) (TC x) (TC y) = TC $ dzipWith (..-) x y


----------------------------------------------------------
-- Convenience

type Sig1 typ = TC Signal typ (UVec Val)
type FSig1 typ = TC FSignal typ (UVec2 Val)

type Sig2 typ = TC Signal typ (Vec Val)
type FSig2 typ = TC FSignal typ (Vec2 Val)

-- type PSig = 

