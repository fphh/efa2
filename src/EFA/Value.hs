{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module EFA.Value where

import EFA.Value.Type as Type
import EFA.Equation.Arithmetic as Arith

-- | Conversion to Double
class ToDouble a where 
  toDouble :: a -> Double  


instance ToDouble Double where   
  toDouble=id

instance ToDouble Rational where   
  toDouble=Arith.fromRational

instance (Constant a, GetDynamicType (TC efa phy a),ToDouble a) => ToDouble (TC efa phy a) where 
  toDouble (TC x) = toDouble x
  
{-
class ToDisplayData efa phy a where  
  toDisplayUnit :: TC efa phy a -> a
  
instance (Constant a, GetDynamicType (TC efa phy a)) => ToDisplayData efa phy a where 
  toDisplayUnit tx@(TC x) = (Arith.fromRational scale) Arith.~* x
    where (UnitScale scale) =  Type.getUnitScale $ Type.getDisplayUnit $ Type.getDynamicType tx
-}

-- | DataType to provide Min and Max Value  
data Range a = Range {getMin :: a, 
                      getMax :: a} deriving (Show,Eq) 
  
instance Functor Range where
  fmap f (Range min max) = Range (f min) (f max)
  
instance (GetDynamicType a) => Type.GetDynamicType (Range a) where   
  getDynamicType = Type.getDynamicType . getMin
  
  
combineRange :: Ord a => Range a -> Range a -> Range a  
combineRange (Range mi ma) (Range mi1 ma1) = Range (min mi mi1) (max ma ma1) 

