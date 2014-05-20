{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module EFA.Value where

import EFA.Value.Type as Type
import EFA.Equation.Arithmetic as Arith
{-
-- | Conversion to Double
class ToDouble a where 
  toDouble :: a -> Double  


instance ToDouble Double where   
  toDouble=id

instance ToDouble Rational where   
  toDouble=Arith.fromRational

instance (Constant a, GetDynamicType (TC efa phy a),ToDouble a) => ToDouble (TC efa phy a) where 
  toDouble (TC x) = toDouble x
-}  

toDisplayUnit' :: (Constant a) => Type.Dynamic -> a -> a
toDisplayUnit' typ x = (Arith.fromRational scale) Arith.~* x
  where (UnitScale scale) = Type.getUnitScale $ Type.getDisplayUnit typ

instance (Constant a,Product (TC efa phy a)) => Arith.Constant (TC efa phy a) where
  zero = TC $ Arith.zero 
  fromRational = TC . Arith.fromRational
  fromInteger = TC . Arith.fromInteger

instance (Product a, Sum (TC efa phy a)) => Arith.Product (TC efa phy a) where
  (~*) (TC x) (TC y) = TC $ x Arith.~* y
  (~/) (TC x) (TC y) = TC $ x Arith.~/ y
  recip (TC x) = Arith.recip (TC x)
  constOne (TC x) = TC (Arith.constOne x)

instance Sum a => Arith.Sum (TC efa phy a) where
  (~+) (TC x) (TC y) = TC $ x Arith.~+ y
  (~-) (TC x) (TC y) = TC $ x Arith.~- y
  negate (TC x) = Arith.negate (TC x)



class ToDisplayUnit a where  
--  toDisplayUnit :: TC efa phy a -> TC efa phy a
  toDisplayUnit :: a -> a
{-  
instance (Constant a, GetDynamicType (TC efa phy a)) => ToDisplayUnit efa phy a where 
  toDisplayUnit tx@(TC x) = TC $ toDisplayUnit' (Type.getDynamicType tx) x
-}

instance (Constant a, GetDynamicType (TC efa phy a)) => ToDisplayUnit (TC efa phy a) where 
  toDisplayUnit tx@(TC x) = TC $ toDisplayUnit' (Type.getDynamicType tx) x

-- | DataType to provide Min and Max Value  
data Range a = Range {getMin :: a, 
                      getMax :: a} deriving (Show,Eq) 
  
instance Functor Range where
  fmap f (Range min max) = Range (f min) (f max)
  
instance (GetDynamicType a) => Type.GetDynamicType (Range a) where   
  getDynamicType = Type.getDynamicType . getMin
  
  
combineRange :: Ord a => Range a -> Range a -> Range a  
combineRange (Range mi ma) (Range mi1 ma1) = Range (min mi mi1) (max ma ma1) 

