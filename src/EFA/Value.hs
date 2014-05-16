module EFA.Value where

import EFA.Value.Type as Type

-- | Conversion to Double
class ToDouble a where 
  toDouble :: a -> Double  
  
instance ToDouble Double where   
  toDouble=id

-- | DataType to provide Min and Max Value  
data Range a = Range {getMin :: a, 
                      getMax :: a} deriving (Show,Eq) 
  
instance Functor Range where
  fmap f (Range min max) = Range (f min) (f max)
  
instance (GetDynamicType a) => Type.GetDynamicType (Range a) where   
  getDynamicType = Type.getDynamicType . getMin
  
  
combineRange :: Ord a => Range a -> Range a -> Range a  
combineRange (Range mi ma) (Range mi1 ma1) = Range (min mi mi1) (max ma ma1) 

instance (ToDouble a) => ToDouble (Type.TC efa phy a) where 
  toDouble (TC x) = toDouble x
