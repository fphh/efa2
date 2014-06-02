{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE  FlexibleContexts #-}

module EFA.Value where

import EFA.Value.Type as Type
--import EFA.Equation.Arithmetic as Arith
import EFA.Data.Vector as DV 


-- | DataType to provide Min and Max Value
data Range a = Range {getMin :: a,
                      getMax :: a} deriving (Show,Eq)

instance Functor Range where
  fmap f (Range mi ma) = Range (f mi) (f ma)

instance (GetDynamicType a) => Type.GetDynamicType (Range a) where
  getDynamicType = Type.getDynamicType . getMin


getValueRange :: (Ord a, Storage vec a, Singleton vec) => vec a -> Range a
getValueRange vec = Range mi ma
  where (mi, ma) = DV.minmax vec

combineRange :: Ord a => Range a -> Range a -> Range a
combineRange (Range mi ma) (Range mi1 ma1) = Range (min mi mi1) (max ma ma1)

