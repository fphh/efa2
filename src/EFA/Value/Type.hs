{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module EFA.Value.Type where

import EFA.Value.Type.Physical as P
import EFA.Value.Type.Efa as E
-- import EFA.Value as Value
import Graphics.Gnuplot.Value.Tuple as Tuple
import Graphics.Gnuplot.Value.Atom as Atom


newtype TC efa phy a = TC a deriving (Show,Eq,Ord)

instance Tuple.C a => Tuple.C (TC efa phy a) where
  text (TC x) = text x

instance Atom.C (TC efa phy a) where



class GetDynamicType a where
  getDynamicType :: a -> Dynamic

instance GetDynamicType Double where getDynamicType _ = UT
instance GetDynamicType (TC E.T P.P a) where getDynamicType _ = P                                     
instance GetDynamicType (TC E.F P.E a) where getDynamicType _ = E                                     
                                    

data Dynamic = 
  P
  | E
  | UT deriving (Show,Eq)
                    
                    
data Wrapper a =                    
  Wrap_P (TC E.T P.P a) 
  |Wrap_UT a deriving Show
                    
class Wrap e p where                    
  wrap :: TC e p a -> Wrapper a
  
instance Wrap E.T P.P where wrap x = Wrap_P x
                    
class UnWrap e p where                    
  unWrap :: Wrapper a -> TC e p a

instance UnWrap E.T P.P where unWrap (Wrap_P x) = x

data DisplayUnit = Unit_kWh | Unit_Joule | Unit_kJoule | Unit_kW | Unit_Percent | Unit_None | Unit_Sec | Unit_UT | Unit_W

showUnit Unit_kWh  = "kWh"
showUnit Unit_Joule  = "J"
showUnit Unit_kJoule  = "kJ"
showUnit Unit_kW   = "kW"
showUnit Unit_Percent = "%"
showUnit Unit_None = "/"
showUnit Unit_Sec = "s"
showUnit Unit_UT = "UT"
showUnit Unit_W = "W"



getDisplayUnit :: Dynamic -> DisplayUnit
getDisplayUnit P = Unit_kW
getDisplayUnit E = Unit_kWh
getDisplayUnit UT = Unit_UT
getDisplayUnit x = (error $ "Error EFA.Value.Type, getDisplayUnit - not implemented yet: " ++ show x)

newtype UnitScale = UnitScale Rational

-- | get display scale per Unit
getUnitScale :: DisplayUnit -> UnitScale
getUnitScale Unit_kWh = UnitScale (1/1000/3600)
getUnitScale Unit_Joule = UnitScale 1
getUnitScale Unit_kJoule = UnitScale (1/1000)
getUnitScale Unit_None = UnitScale 1
getUnitScale Unit_Percent = UnitScale 100
getUnitScale Unit_Sec = UnitScale 1
getUnitScale Unit_kW = UnitScale (1/1000)
getUnitScale Unit_UT = UnitScale 1
getUnitScale Unit_W = UnitScale 1
