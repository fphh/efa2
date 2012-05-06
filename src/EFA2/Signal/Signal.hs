{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where
import EFA2.Signal.Vector2

--------------------------------
-- type Wrapper 
newtype TC t s e = TC e deriving (Show) 

--------------------------------
-- | Physical Types & Classification
data P 
data DT 
data E 
data N
data M
data X
data BZ 
data IZ
data UZ

class TMult t1 t2 t3 | t1 t2 -> t3 where
    
instance TMult N N N  
instance TMult DT P E 
instance TMult P DT E 


----------------------------------------------------------
-- | EFA data containers

data Scalar 
data Signal 
data Distrib
data FSignal
data FDistrib
data SampleVec
data FSampleVec
data ClassVec

data Norm
data Cross

class Flag s1 s2 h1 h2 h3 | s1 s2 -> h1 h2 h3 where
  
instance Flag Signal Signal Norm Norm Cross
instance Flag Signal SampleVec Norm Cross Cross 

-- Signal Aritmetics
class SArith s1 s2 s3 | s1 s2 -> s3 where -- hv1 hv2 hv3
--  foo :: s1 -> s2 -> s3 -> Int 
  
instance SArith Signal Signal Distrib 

instance SArith Signal SampleVec Signal 
{-
instance SArith FSignal FSignal FSignal Norm Norm Cross 
instance SArith FSampleVec FSignal FSignal Norm Cross Cross 

instance SArith Scalar Signal Signal Norm Norm Cross 

instance SArith Distrib Distrib Distrib Norm Norm Cross 
instance SArith Distrib ClassVec Distrib  Norm Cross Cross 


-}  
-- instance SArith s1 s2 s3 hv1 hv2 hv3 => SArith s2 s1 s3 hv1 hv2 hv3

----------------------------------------------------------
-- | Apply Function

-- apply2TC :: (e1 -> e2 -> e3) -> TC t1 s1 e1 -> TC t2 s2 e2 -> TC t3 s3 e3
-- apply2TC f (TC x) (TC y) = TC $ f x y
  
apply2TC :: (e1 -> e2 -> e3) -> TC t1 s1 e1 -> TC t2 s2 e2 -> TC t3 s3 e3
apply2TC f (TC x) (TC y) = TC $ f x y



class  PhysArith h1 h2 h3 t1 t2 t3  s1 s2 s3 e1 | t1 t2 -> t3 where
  (~*) :: TC t1 s1 e1 -> TC t2 s2 e1 -> TC t3 s3 e1 

instance (SArith s1 s2 s3, TMult t1 t2 t3, Flag s1 s2 h1 h1 h3) => PhysArith h1 h1 h3 t1 t2 t3 s1 s2 s3 Val where
  (~*) x y = apply2TC (*) x y

instance (SArith s1 s2 s3, TMult t1 t2 t3, Flag s1 s2 h1 h2 h2) => PhysArith h1 h2 h2 t1 t2 t3 s1 s2 s3 Val where
  (~*) x y = apply2TC (/) x y

{-
instance (SArith s1 s2 s3 Norm, TMult t1 t2 t3) => PhysArith Norm t1 t2 t3 s1 s2 s3 Val where
  (~*) x y = apply2TC (*) x y

instance (SArith s1 s2 s3 Cross, TMult t1 t2 t3) => PhysArith Cross t1 t2 t3 s1 s2 s3 Val where
  (~*) x y = apply2TC (/) x y
-}
{-
instance  ( SArith s1 s2 s3 hv1) => PhysArith h1 P DT E s1 s2 s3 (DC D1 (UVec Val)) (DC D1 (UVec Val)) (DC D1 (UVec Val)) where
  (~*) x y = apply2TC (.*) x y
-}
-- instance  (DArith1 e1 e2 e3, SArith s1 s2 s3 Cross, TMult t1 t2 t3) => PhysArith Cross t1 t2 t3 s1 s2 s3 e1 e2 e3 where
--   (~*) x y = apply2TC (./) x y

-- instance  (PhysArith hv0 P DT t30 Signal Signal s30 (DC D1 (UVec Val)) (DC D1 (UVec Val)) e30) where
--   (~*) x y = apply2TC (./) x y

class Disp t s e where
  disp :: TC t s e -> String

instance Disp P s Val where
  disp x = "P" 
  
-- instance Disp E s Val where  
--   disp x = "E"

instance Disp t SampleVec Val where  
  disp x = "SampleVec"

instance Disp t Signal Val where  
  disp x = "Signal"

instance Disp t Distrib Val where  
  disp x = "Distrib"
