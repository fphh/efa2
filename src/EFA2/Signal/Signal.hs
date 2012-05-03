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

class ET_Not_Num t



class TMult t1 t2 t3 | t1 t2 -> t2 where
instance TMult P DT E

----------------------------------------------------------
-- | 2. EFA data containers

-- data ECont d c 
-- d = derivative
-- c = container

newtype TC t d = TC d deriving (Show) 
{-
-- class TC s 
fromTC :: TC t s d -> s d  
fromTC (TC x) = x

toTC :: s d -> TC t s d
toTC x = TC x

class TMult t1 t2 t3 => TzipWith s1 s2 s3 t1 t2 t3 d1 d2 d3 where
  tzipWith :: (d1 -> d2 -> d3) -> TC t1 s1 d1 -> TC t3 s2 d2 -> TC t3 s3 d3  
  tzipWith f x y = toTC $ ezipWith f (fromTC x) (fromTC y)
  
instance  TMult t1 t2 t3 => TzipWith s1 s2 s3 t1 t2 t3 d1 d2 d3

-}
  

apply2 :: (d1 -> d2 -> d3) -> TC t1 d1 -> TC t2 d2 -> TC t3 d3
apply2 f (TC x) (TC y) = TC $ f x y
  
  
-- (~*) :: TMult t1 t2 t3 => TC t1 d1 -> TC t2 d2 -> TC t3 d3
(~*) x y = apply2 (.*) x y 


