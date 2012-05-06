{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures,IncoherentInstances, GeneralizedNewtypeDeriving,FlexibleContexts #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where
import EFA2.Signal.Vector2

--------------------------------
-- type Wrapper 
newtype TC h t s e = TC e deriving (Show) 

--------------------------------
-- | Physical Types & Classification
data P 
data DT
data E 
data N
data BZ 
data IZ
data UZ

class TMult t1 t2 t3 | t1 t2 -> t3 where
instance TMult P DT E
instance TMult E N  E

class  TDiv t1 t2 t3
instance TMult t1 t2 t3 => TDiv t3 t2 t1 

----------------------------------------------------------
-- | Position

data S
data H
data V
data H2
data V2


----------------------------------------------------------
-- | EFA data containers

data Scalar
data Signal
data Distrib
data FSignal
data FDistrib

class SArith s1 s2 s3 | s1 s2 -> s3 where
instance SArith Signal Signal Signal
instance SArith Scalar Signal Signal

instance SArith FSignal FSignal FSignal
instance SArith Scalar FSignal FSignal

instance SArith s1 s2 s3 => SArith s2 s1 s3 
  
----------------------------------------------------------
-- | Apply Function

apply2EC :: (e1 -> e2 -> e3) -> TC h1 t1 s1 e1 -> TC h2 t2 s2 e2 -> TC h3 t3 s3 e3
apply2EC f (TC x) (TC y) = TC $ f x y
  

class HArith h1 h2 h3 | h1 h2 -> h3
instance HArith S S S
instance HArith S H H
instance HArith S V V
instance HArith V V V
instance HArith H H2 H2
instance HArith V V2 V2 
instance HArith H2 H2 H2
instance HArith V2 V2 V2

instance HArith h1 h2 h3 => HArith h2 h1 h3 


class (DCMult e1 e2 e3, SArith s1 s2 s3, TMult t1 t2 t3 ) => PhysArith h1 t1 s1 e1 h2 t2 s2 e2 h3 t3 s3 e3 | h1 h2 -> h3 where
  (~*) :: TC h1 t1 s1 e1 -> TC h2 t2 s2 e2 -> TC h3 t1 s3 e3 
  (~*) x y = apply2EC (.*) x y

instance  (DCMult e1 e2 e3, SArith s1 s2 s3, TMult t1 t2 t3,HArith h1 h2 h3) => PhysArith S t1 s1 e1 h2 t2 s2 e2 h3 t3 s3 e3
instance  (DCMult e1 e2 e3, SArith s1 s2 s3, TMult t1 t2 t3,HArith h1 h2 h3) => PhysArith H t1 s1 e1 H t2 s2 e2 h3 t3 s3 e3
instance  (DCMult e1 e2 e3, SArith s1 s2 s3, TMult t1 t2 t3,HArith h1 h2 h3) => PhysArith V t1 s1 e1 V t2 s2 e2 h3 t3 s3 e3
-- instance  (CMult e1 e2 e3, SArith s1 s2 s3, TMult t1 t2 t3,HArith h1 h2 h3) => PhysArith V t1 s1 e1 H t2 s2 e2 h3 t3 s3 e3





