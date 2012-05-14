{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-}

-- UndecidableInstances, OverlappingInstances

module EFA2.Signal.Typ (module EFA2.Signal.Typ) where


--------------------------------
-- Type Variable
data Typ d t p

-- d = delta flag
-- t = EFA Typ
-- p = partial flag
-- a = average flag

--------------------------------
-- | Physical Types & Classification

-- Time Variables
data T -- Time
data P

-- Flow Variable (Edges)
-- data PF -- Power 
data F -- Energy Flow is F = DE ?
data N -- Flow Efficiency
data X -- Flow Divider
data Y -- Flow Merger

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

-------------------------------------
-- | Partial Flag

data Tt -- Total 
data Pt -- Partial


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
-- Typ aendert sich 
-- delta und partial spielen hin und wieder eine Rolle

class TProd t1 t2 t3 | t1 t2 -> t3, t2 t3 -> t1, t1 t3 -> t1 

-- F = P*dt - Flow and Power
-- Time to Flow
instance  TProd (Typ D T p) (Typ A P p) (Typ A F p)
instance TProd (Typ A P p) (Typ D T p) (Typ A E p)

-- F=N*F -- Flow and Flow Efficiency
instance TProd (Typ d F p) (Typ d N p) (Typ d F p) 
instance TProd (Typ d N p) (Typ d F p) (Typ d F p) 

-- E=M*E -- Energy mix and Mix Part
instance TProd (Typ d E Tt) (Typ d M Tt) (Typ d E Pt)
instance TProd (Typ d M Tt) (Typ d E Tt) (Typ d E Pt)

-- F=X*FO -- Energy mix and Mix Part
instance TProd (Typ d FO Tt) (Typ d X Tt) (Typ d F Pt)
instance TProd (Typ d X Tt) (Typ d FO Tt) (Typ d F Pt)

-- F=Y*FI -- Energy mix and Mix Part
instance TProd (Typ d FI Tt) (Typ d Y Tt) (Typ d F Pt)
instance TProd (Typ d Y Tt) (Typ d FI Tt) (Typ d F Pt)


-- Addition & Subtraction

-- Alles muss Identisch sein, nur Delta veraendert sich

class TSum t1 t2 t3 |  t1 t2 -> t3, t2 t3 -> t1, t1 t3 -> t1 
  
instance TSum (Typ A t p) (Typ D t p) (Typ A t p)
instance TSum (Typ D t p) (Typ A t p) (Typ A t p)

instance TSum (Typ D t p) (Typ DD t p) (Typ D t p)
instance TSum (Typ DD t p) (Typ D t p) (Typ D t p)

instance TSum (Typ DD t p) (Typ DDD t p) (Typ DD t p)
instance TSum (Typ DDD t p) (Typ DD t p) (Typ DD t p)


