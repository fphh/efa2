{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where
import EFA2.Signal.Vector2

--------------------------------
-- type Wrapper 
newtype TC t s h e = TC e deriving (Show) 

data V
data H

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

----------------------------------------------------------
-- | Orientation


-- data Norm
-- data Cross

-- class Flag s1 s2 | s1 s2 -> h1 h2 h3 where
  
-- instance Flag Signal Signal Norm Norm Cross
-- instance Flag Signal SampleVec Norm Cross Cross 

-- Signal Aritmetics
class SArith s1 s2 s3 | s1 s2 -> s3 where -- hv1 hv2 hv3

instance SArith Scalar Scalar Scalar
instance SArith Signal Signal Signal
instance SArith FSignal FSignal FSignal 
instance SArith Distrib Distrib Distrib 

instance SArith Scalar Signal Signal 
instance SArith Signal Scalar Signal 
instance SArith Scalar FSignal FSignal 
instance SArith FSignal Scalar FSignal 
instance SArith Scalar Distrib Distrib 
instance SArith Distrib Scalar Distrib


----------------------------------------------------------
-- | Apply Function

apply2TC :: (e1 -> e2 -> e3) -> TC t1 s1 h1 e1 -> TC t2 s2 h2 e2 -> TC t3 s3 h3 e3
apply2TC f (TC x) (TC y) = TC $ f x y

class  PhysArith h1 h2 h3 t1 t2 t3 s1 s2 s3 e1 | t1 t2 -> t3, h1 h2 -> h3 where
  (~*) :: TC t1 s1 h1 e1 -> TC t2 s2 h2 e1 -> TC t3 s3 h3 e1 

instance (SArith s1 s2 s3, TMult t1 t2 t3) => PhysArith H H H t1 t2 t3 s1 s2 s3  Val where
  (~*) x y = apply2TC (*) x y

instance (SArith s1 s2 s3, TMult t1 t2 t3) => PhysArith H V H t1 t2 t3 s1 s2 s3  Val where
  (~*) x y = apply2TC (/) x y

instance (SArith s1 s2 s3, TMult t1 t2 t3) => PhysArith V H V t1 t2 t3 s1 s2 s3  Val where
  (~*) x y = apply2TC (/) x y

instance (SArith s1 s2 s3, TMult t1 t2 t3) => PhysArith V V V t1 t2 t3 s1 s2 s3  Val where
  (~*) x y = apply2TC (*) x y

class Disp t s h e where
  disp :: TC t s h e -> String

instance Disp P s h Val where
  disp x = "P" 
  
-- instance Disp E s Val where  
--   disp x = "E"

instance Disp t SampleVec h Val where  
  disp x = "SampleVec"

instance Disp t Signal Val h where  
  disp x = "Signal"

instance Disp t Distrib Val h where  
  disp x = "Distrib"
