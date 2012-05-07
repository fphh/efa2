{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, GeneralizedNewtypeDeriving,FlexibleContexts,OverlappingInstances #-} 


module EFA2.Signal.Base (module EFA2.Signal.Base) where



-- hier zwischen Liste & Vector umschalten ?? -- wie

----------------------------------------------------------
-- | 1. Data types
type Val = Double -- or Ratio
data Sign = PSign | ZSign | NSign deriving (Show, Eq, Ord)


class NeutralElement d where
  neutral :: d
  
instance NeutralElement Val where
  neutral = 0
  
instance NeutralElement Sign where
  neutral = ZSign

instance NeutralElement Int where
  neutral = 0
  
instance NeutralElement Bool where
  neutral = False
  
