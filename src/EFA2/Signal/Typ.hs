{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where
import EFA2.Signal.Vector2

--------------------------------
-- type Wrapper 
newtype TC t s s = TC s deriving (Show) 

--------------------------------
-- | Physical Types & Classification

-- Flow Variable (Edges)
data P -- Power 
data T -- Time
data F -- Energy Flow
data N -- Flow Efficinecy
data X -- Flow Mix

-- Flow Variables (Knodes) 
data FI -- Sum Flow in
data FO -- Sum Flow out

-- Storage Variables
data E -- Energy Storage or Consumption
data NE -- Efficiency of Stored Energy
data M -- Storage Mix

-- Logic Variables
data BZ -- Bool State
data IZ -- Int State
data UZ -- User Defined State

-------------------------------------
-- | Delta Flag
data A -- Absolut
data D -- Delta
data DD -- Delta Delta
data DDD -- Delta Delta Delta

class Succ d1 d2 | d1 -> d2, d2 -> d1 
instance A D
instance D DD
instance DD DDD

class Prec d1 d2 | d1 -> d2, d2 -> d1 
instance D A
instance DD D
instance DDD DD
  
-------------------------------------
-- | Delta Flag

class TMult t1 t2 t3 | t1 t2 -> t3, t2 t3 -> t1, t1 t3 -> t1 
    
instance TMult (d F) (d F) (d N)  
instance TMult (d E) (d E) (d NE)  
instance TMult (d F) (d FI) (d Y)  
instance TMult (d F) (d FO) (d X)  


instance  Succ d1 d2 => TMult (d2 T) (d1 P) (d1 E)



instance  TMult t1 t2 t3 => TMult t2 t1 t3 

-- Addition & Subtraction
class TSum t1 t2 t3 |  t1 t2 -> t3, t2 t3 -> t1, t1 t3 -> t1 
  
instance Succ d1 d2 => TSum (d1 t) (d2 t) (d1 t)   -- Absolut + Delta = Absolut
instance Succ d1 d2 => TSum (d2 t) (d1 t) (d1 t)   -- Delta + Absolut = Absolut

class TSub t1 t2 t3 |  t1 t2 -> t3, t2 t3 -> t1, t1 t3 -> t1 
instance  TSum t1 t2 t3 => TSub t1 t3 t2 -- Abolut - Abolut  = Delta
  

apply2TC :: (s1 -> s2 -> s3) -> TC t1 s1 -> TC t2 s2 -> TC t3 s3
apply2TC f (TC x) (TC y) = TC $ f x y


class  PhysArith h1 h2 h3 t1 t2 t3 s1 s2 s3 e1 e2 e3 | h1 h2 -> h3, h2 h3 -> h1  where
  (~*) :: TC t1 s1 h1 e1 -> TC t2 s2 h2 e2 -> TC t3 s3 h3 e3 

instance (DArith1 e1 e2 e3, SArith s1 s2 s3, TMult t1 t2 t3) => PhysArith H H H t1 t2 t3 s1 s2 s3 e1 e2 e3  where
  (~*) x y = apply2TC (.*) x y

instance (DArith1 e1 e2 e3, SArith s1 s2 s3, TMult t1 t2 t3) => PhysArith H V H t1 t2 t3 s1 s2 s3 e1 e2 e3  where
  (~*) x y = apply2TC (./) x y

instance (DArith1 e1 e2 e3, SArith s1 s2 s3, TMult t1 t2 t3) => PhysArith V H V t1 t2 t3 s1 s2 s3  e1 e2 e3 where
  (~*) x y = apply2TC (./) x y

instance (DArith1 e1 e2 e3, SArith s1 s2 s3, TMult t1 t2 t3) => PhysArith V V V t1 t2 t3 s1 s2 s3 e1 e2 e3 where
  (~*) x y = apply2TC (.*) x y


