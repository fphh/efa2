{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances, OverlappingInstances #-}

module EFA2.Signal.Typ (module EFA2.Signal.Typ) where


--------------------------------
-- type Wrapper 
data Typ d p 

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

{-
class Succ d1 d2 | d1 -> d2, d2 -> d1 
instance Succ A D
instance Succ D DD
instance Succ DD DDD

class Prec d1 d2 
instance Succ d1 d2 => Prec d2 d1
-}  
-------------------------------------
--  |Type Arithmetic

class TProd t1 t2 t3 | t1 t2 -> t3, t2 t3 -> t1, t1 t3 -> t1 
    
instance  TProd (Typ D T) (Typ A P) (Typ A E)
instance TProd (Typ d F) (Typ d N) (Typ d F) 

instance TProd (Typ d E) (Typ d M) (Typ d E)
--instance TProd (Typ d E) (Typ d M) (Typ d E)



-- Addition & Subtraction
class TSum t1 t2 t3 |  t1 t2 -> t3, t2 t3 -> t1, t1 t3 -> t1 
  
instance TSum (Typ A t) (Typ D t) (Typ A t)
instance TSum (Typ D t) (Typ A t) (Typ A t)

instance TSum (Typ D t) (Typ DD t) (Typ D t)
instance TSum (Typ DD t) (Typ D t) (Typ D t)

instance TSum (Typ DD t) (Typ DDD t) (Typ DD t)
instance TSum (Typ DDD t) (Typ DD t) (Typ DD t)


