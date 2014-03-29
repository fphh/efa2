{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module EFA.Data.Type where

--import EFA.Data.Type.Physical
--import EFA.Data.Type.Efa

newtype TC p f e a = TC a 

instance Functor (TC p f e) where
  fmap f (TC x) = TC $ f x  

{-
data Wrap a = WrapPower (TC Power Time Edge a) |
              WrapFlow (TC Energy Flow Edge a) |
              WrapStoEnergy (TC Energy Cum Sto a)


class WrapIt p f e where wrap :: TC p f e a  -> Wrap a
instance WrapIt Power Time Edge where wrap x = WrapPower x
instance WrapIt Energy Flow Edge where wrap x = WrapFlow x

class UnWrap p f e where unwrap :: Wrap a -> TC p f e a
instance UnWrap Power Time Edge where unwrap (WrapPower x) = x
instance UnWrap Energy Flow Edge where unwrap (WrapFlow x) = x 

-}