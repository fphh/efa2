{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module EFA.Value.Type where

import qualified EFA.Value.Type.Physical as P
import qualified EFA.Value.Type.Efa as E
import qualified EFA.Equation.Arithmetic as Arith
-- import EFA.Value as Value
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom

import EFA.Utility(Caller,
--  Caller(..),merror,(|>),
  ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "EFA.Value.Type"

nc:: FunctionName -> Caller
nc = genCaller modul

newtype TC efa phy a = TC a deriving (Show,Eq,Ord)


-- TODO .. Arith Klasse darf nur für UT gelten !!
instance (Arith.Constant a,Arith.Product (TC efa P.UT a)) => Arith.Constant (TC efa phy a) where
  zero = TC $ Arith.zero
  fromRational = TC . Arith.fromRational
  fromInteger = TC . Arith.fromInteger

instance (Arith.Product a, Arith.Sum (TC efa phy a)) => Arith.Product (TC efa phy a) where
  (~*) (TC x) (TC y) = TC $ x Arith.~* y
  (~/) (TC x) (TC y) = TC $ x Arith.~/ y
  recip (TC x) = Arith.recip (TC x)
  constOne (TC x) = TC (Arith.constOne x)

instance Arith.Sum a => Arith.Sum (TC efa phy a) where
  (~+) (TC x) (TC y) = TC $ x Arith.~+ y
  (~-) (TC x) (TC y) = TC $ x Arith.~- y
  negate (TC x) = Arith.negate (TC x)

instance Tuple.C a => Tuple.C (TC efa phy a) where
  text (TC x) = Tuple.text x

instance Atom.C (TC efa phy a) where



class GetDynamicType a where
  getDynamicType :: a -> Dynamic

instance GetDynamicType Double where getDynamicType _ = UT
instance GetDynamicType (TC E.T P.P a) where getDynamicType _ = P
instance GetDynamicType (TC E.F P.E a) where getDynamicType _ = E


data Dynamic =
  P
  | E
  | N  
  | T  
  | UD 
  | UT deriving (Show,Eq)


data Wrapper a =
  Wrap_P (TC E.T P.P a)
  |Wrap_UT a deriving Show

class Wrap e p where
  wrap :: TC e p a -> Wrapper a

instance Wrap E.T P.P where wrap x = Wrap_P x
-- instance Wrap E.T P.UT where wrap x = Wrap_UT x

class UnWrap e p where
  unWrap :: Wrapper a -> TC e p a

-- TODO: roll out with correct physical typing
instance UnWrap E.T P.P where unWrap (Wrap_P x) = x
-- instance UnWrap E.T P.UT where unWrap (Wrap_UT x) = x

data DisplayUnit = Unit_kWh 
                 | Unit_Joule 
                 | Unit_kJoule 
                 | Unit_kW 
                 | Unit_Percent 
                 | Unit_None 
                 | Unit_Sec 
                 | Unit_UT 
                 | Unit_W 
                 | Unit_s  
                 | Unit_Undefined deriving Show

toDisplayUnit' :: (Arith.Constant a) => Dynamic -> a -> a
toDisplayUnit' typ x = (Arith.fromRational scal) Arith.~* x
  where (UnitScale scal) = getUnitScale $ getDisplayUnit typ

class ToDisplayUnit a where
  toDisplayUnit :: a -> a

instance ToDisplayUnit Double where
  toDisplayUnit x = x

instance (Arith.Constant a, GetDynamicType (TC efa phy a)) => ToDisplayUnit (TC efa phy a) where
  toDisplayUnit tx@(TC x) = TC $ toDisplayUnit' (getDynamicType tx) x


showUnit :: DisplayUnit -> String
showUnit x = case x of
 Unit_kWh  -> "kWh"
 Unit_Joule  -> "J"
 Unit_kJoule  -> "kJ"
 Unit_kW   -> "kW"
 Unit_Percent -> "%"
 Unit_None -> "/"
 Unit_Sec -> "s"
 Unit_UT -> "UT"
 Unit_W -> "W"
 Unit_s -> "s"
 Unit_Undefined -> "#"
 -- _ -> error " EFA.Value.Type/showUnit - not implemented:" ++ show x



getDisplayUnit :: Dynamic -> DisplayUnit
getDisplayUnit P = Unit_kW
getDisplayUnit E = Unit_kWh
getDisplayUnit UT = Unit_UT
getDisplayUnit N = Unit_None
getDisplayUnit UD = Unit_Undefined
getDisplayUnit T = Unit_s
getDisplayUnit x = (error $ "Error EFA.Value.Type/getDisplayUnit - not implemented: " ++ show x)

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
getUnitScale Unit_Undefined = UnitScale 1
getUnitScale Unit_s =  UnitScale 1