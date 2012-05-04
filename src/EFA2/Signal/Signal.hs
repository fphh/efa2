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





----------------------------------------------------------
-- | 2. EFA data containers

-- data ECont d c 
-- d = derivative
-- c = container


newtype TC t c d = TC (c d) deriving (Show) 

class TCont a t c d where
  fromTC ::  a t c d -> c d
  toTC :: c d -> a t c d 
  
instance TCont TC t c d where  
  fromTC (TC x) = x 
  toTC x = TC x 



apply2 :: (TCont a1 t1 c1 d1, TCont a2 t2 c2 d2, TCont a3 t3 c3 d3)  => (c1 d1 -> c2 d2 -> c3 d3) -> a1 t1 c1 d1 -> a2 t2 c2 d2 -> a3 t3 c3 d3
apply2 f x y = toTC $ f (fromTC x) (fromTC y)
  
  
(~*) :: (TMult t1 t2 t3, DMult d1 d2 d3, EZipWith c1 c2 c3 d1 d2 d3, Show (c1 d1), Show (c2 d2)) => TC t1 c1 d1 -> TC t2 c2 d2 -> TC t3 c3 d3 
(~*) x y = apply2 (.*) x y 

{-
newtype TC t e = TC e deriving (Show) 

class (EsipWith c1 c2 c3 d1 d2 d3) =>  Apply2 t1 t2 t3 e1 e2 e3 where 
  apply2 :: (e1 -> e2 -> e3) -> TC t1 e1 -> TC t2 e2 -> TC t3 e3
  apply2 f (TC x) (TC y) = TC $ f x y
  
  
(~*) :: (TMult t1 t2 t3) => TC t1 e1 -> TC t2 e2 -> TC t3 e3
(~*) x y = apply2 (.*) x y 
-}