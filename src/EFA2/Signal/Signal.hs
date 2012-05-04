{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures,IncoherentInstances, GeneralizedNewtypeDeriving,FlexibleContexts #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where

import EFA2.Signal.Vector

--------------------------------
-- type Wrapper 

--d = delta
--t = type 
--p = partial
--c = composition 

-- | Physical Types & Classification
data P
data DT
data E
data Z
data IZ
data UZ

class TMult t1 t2 t3 | t1 t2 -> t2 where
instance TMult P DT E


-- class SMult s1 s2 s3 | s1 s2 -> s3 where
-- instance SMult Signal Signal Signal
-- instance SMult Signal Scalar Signal
-- instance SMult s1 s2 s3 => SMult s2 s1 s3 
  
newtype Signal t c d = Signal (c d) deriving Show
newtype Scalar t c d = Scalar (c d) deriving Show
newtype Distrib t c d = Distrib (c d) deriving Show

----------------------------------------------------------
-- | 2. EFA data containers

-- data ECont d c 
-- d = derivative
-- c = container


-- newtype TC t c d = TC (c d) deriving (Show) 

class TCont s t c d where
  fromTC ::  s t c d -> c d
  toTC :: c d -> s t c d 
  
instance TCont Signal t c d where  
  fromTC (Signal x) = x 
  toTC x = Signal x 

instance TCont Scalar t c d where  
  fromTC (Scalar x) = x 
  toTC x = Scalar x 

instance TCont Distrib t c d where  
  fromTC (Distrib x) = x 
  toTC x = Distrib x 

class (TCont s1 t1 c1 d1, TCont s2 t2 c2 d2, TCont s3 t3 c3 d3, EZipWith c1 c2 c3 d1 d2 d3) => Apply2 s1 s2 s3 t1 t2 t3 c1 c2 c3 d1 d2 d3 where 
      apply2EC ::  (c1 d1 -> c2 d2 -> c3 d3) -> s1 t1 c1 d1 -> s2 t2 c2 d2 -> s3 t3 c3 d3
      apply2EC f x y = toTC $ f (fromTC x) (fromTC y)
      
instance (TCont s1 t1 c1 d1, TCont s2 t2 c2 d2, TCont s3 t3 c3 d3,Show (c1 d1), Show (c2 d2)) => Apply2 Signal Signal Signal t1 t2 t3 c1 c2 c3 d1 d2 d3
      

(~*) :: (TMult t1 t2 t3, DMult d1 d2 d3,Apply2 s1 s2 s3 t1 t2 t3 c1 c2 c3 d1 d2 d3,TCont s1 t1 c1 d1, TCont s2 t2 c2 d2, TCont s3 t3 c3 d3) => s1 t1 c1 d1 -> s2 t2 c2 d2 -> s3 t3 c3 d3 
(~*) x y = apply2EC (.*) x y 



-- ############### Geht Das ? #################
{-
newtype TC t e = TC e deriving (Show) 

class TCont a t e where
  fromTC ::  a t e -> e
  toTC :: c d -> a t e
  
instance TCont TC t e where  
  fromTC (TC x) = x 
  toTC x = TC x 



apply2 :: (TCont a1 t1 e1, TCont a2 t2 e2, TCont a3 t3 e3)  => (e1 -> e2 -> e3) -> a1 t1 e1 -> a2 t2 e2 -> a3 t3 e3
apply2 f x y = toTC $ f (fromTC x) (fromTC y)
  
  
-- (~*) :: TC t1 e1 -> TC t2 e2 -> TC t3 e3
(~*) x y = apply2 (.*) x y 
-}
-- ############### Traumvorstellung #################
