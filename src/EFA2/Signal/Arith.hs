{-# LANGUAGE TypeSynonymInstances #-}

module EFA2.Signal.Arith where


type Val = Double
data Signal = Signal [Val] deriving (Show,Eq)
type Power = Signal
type Time = Signal

class Arith a where
      zero :: a
      cst :: Double -> a
      neg :: a -> a
      rec :: a -> a
      (.+) :: a -> a -> a
      (.*) :: a -> a -> a
      (./) :: a -> a -> a

instance Arith Val where
         zero = 0.0
         cst = id
         neg = negate
         rec = recip
         (.+) = (+)
         (.*) = (*)
         (./) = (/)

-- instance Arith Signal where
-- --         zero = map (*0.0)
--          cst = id
--          neg = map negate
--          rec = map recip
--          (.+) = zipWith (+)
--          (.*) = zipWith (*)
--          (./) = zipWith (/)
