{-# LANGUAGE TypeSynonymInstances #-}

module EFA2.Signal.Arith where


type Val = Double


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