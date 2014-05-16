{-# LANGUAGE MultiParamTypeClasses #-}

module EFA.Value.Type where

import EFA.Value.Type.Physical as P
import EFA.Value.Type.Efa as E

newtype TC efa phy a = TC a deriving Show

class GetDynamicType a where
  getDynamicType :: a -> Dynamic

instance GetDynamicType Double where getDynamicType _ = UT

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